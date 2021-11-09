;;; custom/clojure/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure-cider-lookup-definition (identifier)
  "A lookup handler for `cider-mode'.

This is necessary to fix `cider-find-dwim's inability to capture the full symbol
at point."
  (cider-find-dwim identifier))


;;
;;; Commands

;;;###autoload
(defun +clojure/open-repl (&optional arg type)
  "Open a Cider REPL for clojure and return the buffer."
  (interactive "P")
  ;; TODO Better error handling
  ;; type is `clj' for clojure and `cljs' for clojurescript
  ;; ... with no type specified, assume `clj'.
  (let ((type (or type 'clj)))
    (if-let (buffer (cider-current-repl type))
        (pop-to-buffer buffer)
      (let ((process (cond ((eq type 'clj) (cider-jack-in-clj arg))
                           ((eq type 'cljs) (cider-jack-in-cljs arg)))))
        (message "Starting CIDER server for the first time...")
        (while (and (process-live-p process)
                    (not (cider-current-repl type)))
          (sit-for 1))
        (message "Starting CIDER server for the first time...done")
        (pop-to-buffer (cider-current-repl type))))))

;;;###autoload
(defun +clojure/open-cljs-repl (&optional arg)
  "Open a Cider REPL for clojurescript and return the buffer."
  (interactive "P")
  (+clojure/open-repl arg 'cljs))

;;;###autoload
(defun +clojure/cider-switch-to-repl-buffer-and-switch-ns ()
  "TODO"
  (interactive)
  (cider-switch-to-repl-buffer t))

;;;###autoload
(defun cider-switch-to-nrepl-buffer ()
  "Calls cider-find-and-clear-repl-output interactively with C-u prefix
set so that it clears the whole REPL buffer, not just the output."
  (interactive)
  (when-let ((nrepl-buf (nrepl-make-buffer-name
                         (nrepl--make-hidden-name nrepl-server-buffer-name-template)
                         nil :no-dup)))
    (switch-to-buffer-other-window nrepl-buf)))

;;;###autoload
(defun cider-clear-repl-buffers ()
  "Clears both repl and nrepl output"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'cider-find-and-clear-repl-output)
    (when-let ((nrepl-buf (nrepl-make-buffer-name
                      (nrepl--make-hidden-name nrepl-server-buffer-name-template)
                      nil :no-dup)))
      (set-buffer nrepl-buf)
      (comint-clear-buffer))))

;;;###autoload
(defun clj-fully-qualified-symbol-at-point (&optional for-req)
  "Gets fully qualified Clojure symbol at point. If FOR-REQ argument passed
gets the name suitable for :require of ns declaration."
  (interactive "P")
  (flet ((use-results (x)
                      (message x)
                      (kill-new x)
                      x))
    (let ((sym (cond ((lsp--capability :hoverProvider)
                      (let ((s (-some->> (lsp--text-document-position-params)
                                 (lsp--make-request "textDocument/hover")
                                 (lsp--send-request)
                                 (gethash "contents")
                                 (gethash "value"))))
                        (string-match "\\(```.*\n\\)\\(\\([[:word:]]\\|[[:graph:]]\\)*\\)" s)
                        (string-trim (match-string 2 s))))

                     ((cider-connected-p)
                      (let ((cb (lambda (x)
                                  (when-let ((v (nrepl-dict-get x "value"))
                                             (s (replace-regexp-in-string "[()]" "" v)))
                                    (message (string-trim s))
                                    (kill-new s)))))
                        (cider-interactive-eval
                         (concat "`(" (cider-symbol-at-point t) ")")
                         cb)))
                     (t (message "Neither lsp nor cider are connected")))))
      (if for-req  ; want ns header name, e.g.: "[foo.core :as foo]"
          (if-let* ((m (string-match "^\\(.*\\)\\/" sym))) ; attempt to get anything before the slash
              (let* ((suffix (match-string 1 sym))  ; grab suffix of the symbol i.e. 'foo.core' of 'foo.core/my-thing'
                     ;; grep for '[foo.core :as ...' in the project
                     (grepped (string-trim
                               (shell-command-to-string
                                (format
                                 "rg --glob '*.clj' --max-count 1 --no-filename '\\[%s :as' %s"
                                 suffix
                                 (projectile-project-root))))))
                (if-let* ((m (string-match "\\[.*\\]" grepped))
                          (res (match-string 0 grepped)))
                    (use-results res)
                  (use-results (format "[%s :as ]" suffix))))
            (use-results sym))
        (use-results sym)))))

;;;###autoload
(defun clojure-unalign (beg end)
  "Un-align (remove extra spaces) in vertically aligned sexp around the point."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (clojure-backward-logical-sexp)
                     (list (point) end)))))

  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " "))
      (let ((clojure-align-forms-automatically nil))
       (indent-region beg end)))))

(defun clojure-edn-json-transform (&optional from-json)
  "Transforms EDN to JSON and vice-versa using jet cli.
The direction is determined by current major-mode or can be
explicitly set by universal argument, if present - attemps to
convert from JSON."
  (interactive "P")
  (let* ((from-json* (or from-json (eq major-mode 'json-mode)))
         (region (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (save-excursion
                     (when (looking-at "\{")
                       (forward-char))
                     (let ((end (progn (sp-end-of-sexp)
                                       (point))))
                       (sp-beginning-of-sexp)
                       (backward-char)
                       (list (point) (+ 1 end))))))
         (jet (executable-find "jet"))
         (params (if from-json*
                     '("--from" "json" "--to" "edn" "--keywordize" "--pretty")
                   '("--from" "edn" "--to" "json" "--pretty"))))
    (when (not jet)
      (error "jet cli not found"))
    (save-excursion
      (apply 'call-process-region
             (car region)
             (cadr region)
             jet
             :delete
             '(t nil)
             :display
             params)
      (sp-reindent))))
