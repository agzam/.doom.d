;;; custom/elisp/autoload.el -*- lexical-binding: t; -*-

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
