;;; custom/jira/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun go-jira-browse-ticket-url (ticket)
  "Open TICKET in browser."
  (interactive "sJira ticket number: ")
  (let ((j (go-jira--find-exe))
        (ticket (or ticket
                    (buffer-local-value 'go-jira--ticket-number (current-buffer)))))

    ;; let's not open a new tab if got one in browser already
    (if-let* ((_ (eq system-type 'darwin))
              (ticket-url (go-jira-ticket->url ticket))
              (btab
               (thread-last
                 (browser-get-tabs)
                 (seq-filter
                  (lambda (x)
                    (string= ticket-url (plist-get x :url))))
                 (seq-first)))
              (win-idx (plist-get btab :windowIndex))
              (tab-idx (plist-get btab :tabIndex)))
        (browser-activate-tab win-idx tab-idx)
      (shell-command-to-string (format "%s browse %s" j ticket)))))
