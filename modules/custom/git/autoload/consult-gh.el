;;; custom/git/autoload/consult-gh.el -*- lexical-binding: t; -*-

;;;###autoload
(defun consult-gh-remove-org+ (x)
  (interactive)
  (setq consult-gh--known-orgs-list
        (cl-delete x consult-gh--known-orgs-list :test #'string=))
  (message "Deleted '%s' org from list of orgs." x))

;;;###autoload
(defun consult-gh--view-action+ (cand)
  (interactive)
  (when-let* ((url
               (thread-last
                 '(:repo :issue :pr)
                 (seq-map
                  (lambda (k)
                    (when-let ((x (plist-get (cdr cand) k)))
                      (replace-regexp-in-string
                       " -- .*" ""
                       (substring-no-properties x)))))
                 (apply
                  (lambda (repo issue pr)
                    (format
                     "https://github.com/%s/%s/%s"
                     repo (if issue "issues" "pull")
                     (or issue pr)))))))
    (forge-visit-topic-via-url url)))


