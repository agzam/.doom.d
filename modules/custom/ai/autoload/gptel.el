;;; custom/ai/gptel.el -*- lexical-binding: t; -*-
(defvar +gptel-improve-text-prompt nil)

(defvar +gptel-improve-text-prompts-history
  (list
   (concat "You are a spelling corrector and text improver. "
           "Only correct mistakes, do not alter the text structure unless stylistic, "
           "orthographic, morphologic and other linguistic errors found. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are a fact-checker and text enhancer. "
           "Fix mistakes and flag factual inaccuracies, do not alter the text structure "
           "unless it is absolutely necessary. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing if there were no changes.")

   (concat "You are spelling corrector and text enhancer. "
           "Provide 3 different improved variations of the given text, "
           "separating each variant with: "
           "\n\n---\n\n"
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
  (let* ((last-b (unless arg
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
                     (seq-first))))
         (last-b (or last-b
                     (funcall-interactively
                      #'gptel
                      (format "*%s*" (gptel-backend-name (default-value 'gptel-backend)))))))
    (display-buffer last-b)
    (switch-to-buffer last-b)))

;;;###autoload
(defun gptel-persist-history ()
  "Save buffer to disk when starting gptel"
  (unless (buffer-file-name (current-buffer))
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
  (if-let* ((q (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (if-let* ((s (symbol-at-point)))
                     (symbol-name s) "")))
            (in (concat org-default-folder "/gptel")))
      (let ((consult-ripgrep-args
             (concat consult-ripgrep-args " --sortr=modified")))
        (consult--grep
         "Search in gptel logs: "
         #'consult--ripgrep-make-builder in q))))
