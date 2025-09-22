;;; custom/elisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sp-eval-current-sexp (&optional arg)
  "Eval current sexp."
  (interactive "p")
  (save-excursion
    (let ((evil-move-beyond-eol t))
      (when (looking-at-p "[[({]")
        (forward-char))
      (sp-up-sexp)
      (call-interactively 'eros-eval-last-sexp))))

;;;###autoload
(defun pp-eval-current (&optional arg)
  "Like pp-eval-last-sexp, but for current"
  (interactive "p")
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (goto-char
       (plist-get (or (sp-get-enclosing-sexp)
                      (sp-get-expression)) :end))
      (call-interactively 'pp-eval-last-sexp))))

;;; borrowed from: http://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
;;;###autoload
(defun sharp-quote ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

;;;###autoload
(defun erase-messages-buffer ()
  (interactive)
  (with-current-buffer "*Messages*"
    (read-only-mode -1)
    (erase-buffer)
    (read-only-mode 1)))

;;;###autoload
(defun +switch-to-messages-buffer-other-window ()
  (interactive)
  (setq +last-known-elisp-buffer (current-buffer))
  (if-let ((mwin (get-buffer-window (messages-buffer))))
      (select-window mwin)
    (progn
      (+evil/window-vsplit-and-follow)
      (switch-to-messages-buffer))))

;;;###autoload
(defun +switch-to-last-elisp-buffer ()
  (interactive)
  (when (boundp '+last-known-elisp-buffer)
    (select-window
     (get-buffer-window
      +last-known-elisp-buffer))))

;;;###autoload
(defun +hide-messages-window ()
  (interactive)
  (when-let ((mw (get-buffer-window (messages-buffer))))
    (delete-window mw)))

;;;###autoload
(defun datetime->timestamp (&optional date-string)
  "Converts to Unix time in ms.
Takes datetime string, e.g., 2023-05-21 12:09:31
Returns Unix time in milliseconds (like in Javascript)"
  (interactive)
  (let ((date-str (or date-string (word-at-point))))
    (message
     (* 1000 (car (time-convert (date-to-time date-str) t))))))

;;;###autoload
(defun timestamp->datetime (&optional timestamp)
  "Takes Unix time in milliseconds (like in Javascript) and returns
datetimestring."
  (interactive)
  (let ((ts (or timestamp (string-to-number (word-at-point)))))
    (message
     (format-time-string
      "%Y-%m-%d %H:%M:%S"
      (seconds-to-time
       (string-to-number (substring (number-to-string ts) 0 10)))))))

;;;###autoload
(defun with-editor-eval ()
  "Evaluates current form and pops a buffer with the results.
Usually, the results of evaluation go into *Messages* buffer,
this doesn't change that, it simply copies the relevant log into
its own buffer."
  (interactive)
  (let ((last-pos (with-current-buffer "*Messages*"
                    (point-max))))
    (eval-current-form-sp)
    (let ((log (with-current-buffer "*Messages*"
                 (buffer-substring last-pos (point-max)))))
      (with-current-buffer (get-buffer-create "*eval*")
        (erase-buffer)
        (insert log)
        (switch-to-buffer-other-window (current-buffer))))))

;;;###autoload
(defun elisp-fully-qualified-name ()
  "Returns fully qualified name of a function at point."
  (when-let* ((sym (symbol-at-point))
              (lib (find-function-library 'erase-messages-buffer))
              (ns (file-name-sans-extension
                   (file-name-nondirectory (cdr lib))))
              (name (format "%s/%s" ns sym)))
    name))

;;;###autoload
(defun elisp-fully-qualified-symbol-with-gh-link (&optional main-branch?)
  "Returns a markdown link to line number on GH with a Symbol Name"
  (interactive "P")
  (when-let* ((url (let ((git-link-default-branch
                          (when main-branch? (magit-main-branch))))
                     (git-link-kill)))
              (symbol (elisp-fully-qualified-name))
              (link (format "[%s](%s)" symbol url)))
    (message link)
    (kill-new link)
    link))

;;;###autoload
(defun profiler-report-expand-all ()
  "Expand all entries in the profiler report recursively."
  (interactive)
  (thread-last
    (buffer-list)
    (seq-filter
     (lambda (b)
       (string-match-p
        "\\*\\(CPU\\|Memory\\)-Profiler-Report.*\\*"
        (buffer-name b))))
    (seq-do
     (lambda (b)
       (with-current-buffer b
         (goto-char (point-min))
         (while (not (eobp))
           (profiler-report-expand-entry)
           (profiler-report-next-entry))
         (goto-char (point-min)))))))

;;;###autoload
(defun profiler-report-helpful-symbol-at-point ()
  (interactive)
  (helpful-symbol (get-text-property (point) 'profiler-entry)))


;;;###autoload
(defun info-copy-node-url ()
  "Copy the current Info node's URL to the kill ring."
  (interactive)
  (unless (derived-mode-p 'Info-mode)
    (error "Not in Info mode"))
  (let* ((manual (file-name-sans-extension
                  (file-name-nondirectory Info-current-file)))
         (url (Info-url-for-node (format "(%s)%s" manual Info-current-node))))
    (kill-new url)
    (message "Copied: %s" url)))
