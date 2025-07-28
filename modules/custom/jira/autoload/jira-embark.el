;;; custom/jira/autoload/embark.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +embark-target-jira-ticket-at-point ()
  "Target RFC number at point.
anything like: RFC 123, rfc-123, RFC123 or rfc123."
  (when-let* ((jira-ticket-pattern "\\b[A-Z]+-[0-9]+\\b")
              (bounds (org-in-regexp jira-ticket-pattern 1))
              (beg (car bounds))
              (end (cdr bounds)))
    `(jira-ticket ,(buffer-substring-no-properties beg end)
      . ,(cons beg end))))

(defun jira-find-pull-requests-on-github (&optional jira-ticket orgs)
  "Search for mentioning of JIRA-TICKET on github for given ORGS"
  (interactive)
  (let* ((ticket (or (jira--ticket-arg-or-ticket-at-point jira-ticket)
                     (read-string "Gimme the JIRA ticket to search: "))))
    (github-topics-find-prs ticket)))
