;;; custom/ai/gptel.el -*- lexical-binding: t; -*-

(defvar +llm-mcp--pending-callbacks (make-hash-table :test 'equal)
  "Callbacks waiting for lazy MCP servers to start.")

(defun +llm-mcp-ensure-server (server-name callback)
  "Ensure MCP server SERVER-NAME is connected, then call CALLBACK.
If already connected, CALLBACK fires immediately.  Queues concurrent requests."
  (require 'mcp-hub)
  (cond
   ((gethash server-name mcp-server-connections)
    (funcall callback))
   ;; currently starting - queue
   ((gethash server-name +llm-mcp--pending-callbacks)
    (push callback (gethash server-name +llm-mcp--pending-callbacks)))
   ;; start it
   (t
    (puthash server-name (list callback) +llm-mcp--pending-callbacks)
    (message "Starting MCP server %s..." server-name)
    (mcp-hub-start-all-server
     (lambda ()
       (let ((cbs (gethash server-name +llm-mcp--pending-callbacks)))
         (remhash server-name +llm-mcp--pending-callbacks)
         (if (gethash server-name mcp-server-connections)
             (progn
               (message "MCP server %s ready" server-name)
               (dolist (cb cbs) (funcall cb)))
           (message "MCP server %s failed to start" server-name))))
     (list server-name)))))

(defun +llm-mcp-make-lazy-fn (server-name tool-name arg-names)
  "Create an async tool function that lazily starts SERVER-NAME for TOOL-NAME.
ARG-NAMES is a list of argument name strings for reconstructing the MCP plist."
  (lambda (callback &rest args)
    (+llm-mcp-ensure-server
     server-name
     (lambda ()
       (let ((mcp-args (cl-mapcan
                        (lambda (name val)
                          (list (intern (concat ":" name)) val))
                        arg-names args)))
         (mcp-async-call-tool
          (gethash server-name mcp-server-connections)
          tool-name
          mcp-args
          (lambda (res)
            (funcall callback (mcp--parse-tool-call-result res)))
          (lambda (code message)
            (funcall callback
                     (format "MCP tool %s error: [%s] %s"
                             tool-name code message)))))))))

(defun +llm-extract-tool-defs-from-bb (file)
  "Extract tool definitions from a Babashka MCP server script FILE.
Returns a list of parsed tool definition hash-tables."
  (require 'parseedn)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "(def \\(?:tool-defs?\\|tools\\)\\b" nil t)
      (re-search-forward "[{[]" nil t)
      (backward-char 1)
      (let* ((start (point))
             (_ (forward-sexp 1))
             (edn-str (buffer-substring-no-properties start (point)))
             (result (parseedn-read-str edn-str)))
        (cond
         ((hash-table-p result) (list result))
         ((vectorp result) (append result nil))
         (t nil))))))

(defun +llm-mcp-schema-to-gptel-args (schema)
  "Convert MCP inputSchema hash-table to gptel args format."
  (let* ((properties (gethash :properties schema))
         (required (when-let* ((r (gethash :required schema)))
                     (append r nil)))
         (args '()))
    (when properties
      (maphash
       (lambda (key val)
         (let* ((name (substring (symbol-name key) 1))
                (arg (list :name name
                           :type (gethash :type val "string")
                           :description (or (gethash :description val) ""))))
           (unless (member name required)
             (setq arg (append arg '(:optional t))))
           (push arg args)))
       properties))
    (nreverse args)))

;;;###autoload
(defun +llm-register-mcp-tools-lazy ()
  "Register MCP tools with gptel from server script schemas.
Reads tool definitions directly from .bb files using parseedn.
Tools appear in gptel immediately; servers start lazily on first use."
  (dolist (server mcp-hub-servers)
    (let* ((server-name (car server))
           (command (plist-get (cdr server) :command))
           (category (concat "mcp-" server-name))
           (tool-defs (+llm-extract-tool-defs-from-bb command)))
      (dolist (td tool-defs)
        (let* ((tool-name (gethash :name td))
               (description (or (gethash :description td) ""))
               (schema (gethash :inputSchema td))
               (args (when schema (+llm-mcp-schema-to-gptel-args schema))))
          (gptel-make-tool
           :function (+llm-mcp-make-lazy-fn
                      server-name tool-name
                      (mapcar (lambda (a) (plist-get a :name)) args))
           :name tool-name
           :async t
           :description description
           :args args
           :category category))))))

;;;###autoload
(defun +llm-mcp-servers-from-eca-config ()
  "Read MCP servers from ECA config.json, return `mcp-hub-servers' alist.
Parses ~/.config/eca/config.json (shared with ECA and Claude Code CLI)
and converts mcpServers entries to the format expected by `mcp-hub-servers'."
  (require 'json)
  (when-let* ((config-file (expand-file-name "~/.config/eca/config.json"))
              (_ (file-exists-p config-file))
              (json (json-read-file config-file))
              (servers (alist-get 'mcpServers json)))
    (mapcar
     (lambda (entry)
       (let* ((name (symbol-name (car entry)))
              (props (cdr entry))
              (command (alist-get 'command props))
              (env-alist (alist-get 'env props))
              (result (list name :command command)))
         (when env-alist
           (nconc result
                  (list :env
                        (cl-loop for (k . v) in env-alist
                                 nconc (list (intern (concat ":" (symbol-name k))) v)))))
         result))
     servers)))

(defvar +gptel-improve-text-prompt nil)

(defvar +gptel-improve-text-prompts-history
  (list
   (concat "You are a spelling corrector and text improver. "
           "Correct mistakes, but do not alter the text structure unless stylistic, "
           "orthographic, morphologic and other linguistic errors found. "
           "Do not replace hyphens with em-dash, keep the hyphens. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing, if there were no changes.")

   (concat "You are a fact-checker and text enhancer. "
           "Fix mistakes and flag factual inaccuracies, do not alter the text structure "
           "unless it is absolutely necessary. "
           "Do not replace hyphens with em-dash, keep the hyphens. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing, if there were no changes.")

   (concat "You are spelling corrector and text enhancer. "
           "Provide 3 different improved variations of the given text, "
           "separating each variant with: "
           "\n\n---\n\n"
           "Do not use em-dash, instead use hyphens"
           "Do not include any explanations, titles, headers or bullet points "
           "- ONLY plain text of variants, nothing else!")

   (concat "You are an experienced software developer. Explain the given code snippet, "
           "diving into technical details for better understanding. "
           "Suggest a better approach if necessary. "
           "Strive for concise code that is easy-to-reason about. "
           "Optionally, recommend libraries, tools and literature for better "
           "understanding the problem and improving upon it.")

   (concat "You are a great software developer. "
           "You strive for simplicity in your code "
           "that is both concise and easy-to-reason about. "
           "Add comments to the provide code snippet, without changing the code itself."
           "Do not include any headers, titles or explanations outside of the snippet, "
           "keep the three ticks with the language designator (markdown code markup).")))

(defun +replace-region-with-string (replacement buffer beg end)
  "Replace region or buffer content with REPLACEMENT."
  (with-current-buffer buffer
    (delete-region beg end)
    (insert replacement)
    (insert "\n")))

(transient-define-infix +gptel--improve-text-infix-prompt ()
  "Prompt selection for improving text."
  :description "Set prompt"
  :prompt "Prompt: "
  :variable '+gptel-improve-text-prompt
  :class 'transient-lisp-variable
  :key "- RET"
  :format "%k %d"
  :reader (lambda (prompt &rest _)
            ;; usual bs to keep the preserve the list order
            (let* ((comp-table (lambda (cs)
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (display-sort-function . ,#'identity))
                                     (complete-with-action action cs str pred)))))
                   (sel (completing-read
                         prompt
                         (funcall
                          comp-table
                          +gptel-improve-text-prompts-history))))
              (add-to-list '+gptel-improve-text-prompts-history
                           sel)
              sel)))

(transient-define-infix +gptel--improve-text-write-own-prompt ()
  "Custom prompt for improving text."
  :description "Write your own prompt"
  :prompt "Prompt: "
  :variable '+gptel-improve-text-prompt
  :class 'transient-lisp-variable
  :key "i"
  :format "%k %d"
  :reader (lambda (&rest _) (read-string "Prompt: ")))

(require 'gptel-transient)

;;;###autoload
(transient-define-prefix +gptel-improve-text-transient ()
  "Improve region with gptel."
  [:description
   (lambda ()
     (concat
      (or +gptel-improve-text-prompt
          (car +gptel-improve-text-prompts-history)) "\n"))
   [(gptel--infix-provider)
    (+gptel--improve-text-infix-prompt)
    (+gptel--improve-text-write-own-prompt)]
   [""
    ("C-<return>" "Let's go" +gptel-improve-text)]]
  [:hide always
   :class transient-subgroups
   :setup-children
   (lambda (_)
     "easy toggling prompt variations"
     (transient-parse-suffixes
      '+gptel-improve-text-transient
      (thread-last
        (seq-take +gptel-improve-text-prompts-history 5)
        (seq-map-indexed
         (lambda (prompt idx)
           (let ((n (number-to-string (1+ idx))))
             (list
              n (format "Use prompt %s" n)
              (lambda ()
                (interactive)
                (setq +gptel-improve-text-prompt
                      prompt))
              :transient t)))))))])

;;;###autoload
(defun +gptel-improve-text (&optional arg)
  (interactive "P")
  (unless (region-active-p)
    (user-error "no selection"))
  (setq +gptel-improve-text-prompt (or +gptel-improve-text-prompt
                                       (car +gptel-improve-text-prompts-history)))
  (let* ((buffer (current-buffer))
         (beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end))
         (in-place? (string-match-p
                     "fix mistakes\\|correct mistakes\\|simplify"
                     +gptel-improve-text-prompt)))
    (message "beep-bop... checking your crap with %s" gptel-model)
    (gptel-request text
      :system +gptel-improve-text-prompt
      :buffer buffer
      :callback
      (lambda (resp info)
        (let* ((model (let-plist info .data.model)))
          (cond
           (in-place?
            (let* ((_ (+replace-region-with-string resp buffer beg end))
                   (_ (message "¡Ahí está!"))
                   (fst-buf (with-current-buffer (generate-new-buffer (format "* %s 1 *" model))
                              (insert text)
                              (current-buffer)))
                   (snd-buf (with-current-buffer (generate-new-buffer (format "* %s 2 *" model))
                              (insert resp)
                              (current-buffer)))
                   (diff-win (diff fst-buf snd-buf "--text" 'no-async)))

              ;; cleaner diff
              (with-current-buffer (window-buffer diff-win)
                (read-only-mode -1)
                (goto-char (point-min))
                (dolist (r '("^diff.*\n"
                             "^. No newline at end of file\n"
                             "^. No newline at end of file\n"
                             "^Diff finished.*$"))
                  (re-search-forward r nil :noerror)
                  (replace-match ""))
                (visual-line-mode))
              (kill-buffer fst-buf)
              (kill-buffer snd-buf)))

           (t
            (let ((buf (generate-new-buffer (format "* %s *" model))))
              (with-current-buffer buf
                (markdown-mode)
                (insert resp))
              (switch-to-buffer-other-window buf)))))))))

;;;###autoload
(defun gptel-clear-buffer+ ()
  (interactive)
  (let* ((beg-marker (concat "^" (alist-get gptel-default-mode gptel-prompt-prefix-alist)))
         (keep-line (save-excursion
                      (goto-char (point-max))
                      (when (re-search-backward beg-marker nil t)
                        (unless (save-excursion
                                  (forward-line)
                                  (re-search-forward beg-marker nil t))
                          (point))))))
    (delete-region (point-min) keep-line)
    (evil-insert-state)))

;;;###autoload
(defun gptel+ (&optional arg)
  (interactive "P")
  (let ((last-b (unless arg
                  (thread-last
                    (buffer-list)
                    (seq-filter
                     (lambda (buf)
                       (and
                        (buffer-local-value 'gptel-mode buf)
                        (buffer-file-name buf)
                        (not (string-match-p
                              ".*quick.org$"
                              (buffer-file-name buf))))))
                    (seq-sort
                     (lambda (a b)
                       (string> (buffer-name a) (buffer-name b))))
                    (seq-first)))))
    (if last-b
        (progn
          (display-buffer last-b)
          (switch-to-buffer last-b))
      (call-interactively #'gptel-agent))))

;;;###autoload
(defun gptel-persist-history (_beg _end)
  "Save gptel dedicated buffer to disk.
Only acts on buffers with `gptel-mode' active, skipping transient
uses like `gptel-send' from scratch."
  (when (and (bound-and-true-p gptel-mode)
             (not (buffer-file-name (current-buffer))))
    (let ((suffix (format-time-string "%Y-%m-%d-%T" (current-time)))
          (chat-dir (concat org-default-folder "/gptel"))
          (ext (replace-regexp-in-string "-mode$" "" (symbol-name gptel-default-mode))))
      (unless (file-directory-p chat-dir)
        (make-directory chat-dir :parents))
      (write-file
       (expand-file-name (concat "gptel-" suffix "." ext) chat-dir)))))


;;;###autoload
(defun gptel-quick-question-buffer ()
  "Opens `a quick question` buffer - an `inbox` buffer for uncategorized
gptel conversations."
  (interactive)
  (let ((buf
         (find-file (concat org-default-folder "/gptel/quick.org"))))
    (when (not (buffer-modified-p buf))
      (+org-goto-bottommost-heading)
      (org-narrow-to-subtree)
      (gptel-mode +1))))

;;;###autoload
(defun gptel-log-find ()
  "Grep for things in gptel log files."
  (interactive)
  (let* ((dir (concat org-default-folder "/gptel"))
         (initial (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                    (or (when-let ((s (symbol-at-point)))
                          (symbol-name s))
                        "^")))
         (consult-ripgrep-args
          (concat consult-ripgrep-args " --sortr=modified"))
         (consult-async-min-input 0))  ; Show results immediately
    (consult-ripgrep dir initial)))
