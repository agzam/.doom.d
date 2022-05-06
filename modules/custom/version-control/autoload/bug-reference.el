;;; custom/version-control/autoload/bug-reference.el -*- lexical-binding: t; -*-

;;;###autoload
(defun init-bug-reference-mode-settings ()
  (setq bug-reference-bug-regexp "\\(\\b\\(PR \\|[Bb]ug \\|[Ii]ssue \\|\\)\\([A-z]+\\)\\/\\([A-z -]+\\)#\\([0-9]+\\)\\)")
  (setq bug-reference-url-format #'bug-reference-url-format-fn))

;;;###autoload
(defun bug-reference-url-format-fn ()
  (let ((type (pcase (match-string-no-properties 2)
                ("PR " "pull")
                ((or "Bug " "bug " "Issue " "issue ") "issues")
                ("" nil)))
        (org (match-string-no-properties 3))
        (project (match-string-no-properties 4))
        (ticket (match-string-no-properties 5)))
    (if type (format "https://github.com/%s/%s/%s/%s" org project type ticket)
      (format "https://github.com/%s/%s/search?q=%s" org project ticket))))
