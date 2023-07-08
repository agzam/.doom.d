;;; custom/git/autoload/consult-gh.el -*- lexical-binding: t; -*-

;;;###autoload
(defun consult-gh-remove-org+ (x)
  (interactive)
  (setq consult-gh--known-orgs-list
        (cl-delete x consult-gh--known-orgs-list :test #'string=))
  (message "Deleted '%s' org from list of orgs." x))

;;;###autoload
(defun consult-gh--issue-view-action+ ()
  (interactive)
  (lambda (cand)
    (when-let ((url (thread-last
                      '(:repo :issue)
                      (seq-map
                       (lambda (k)
                         (plist-get (text-properties-at 0 cand) k)))
                      (apply #'format "https://github.com/%s/issues/%s"))))
      (funcall-interactively #'forge-visit-topic-via-url url))))
