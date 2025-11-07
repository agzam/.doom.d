;;; custom/jira/autoload/jira-eldoc.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Eldoc integration to show Jira ticket descriptions on hover in normal mode

;;; Code:

(defvar jira-eldoc-cache (make-hash-table :test 'equal)
  "Cache for Jira ticket descriptions to avoid repeated API calls.")

(defvar jira-eldoc-cache-ttl 3600
  "Time-to-live for cache entries in seconds (default: 1 hour).")

(defun jira-eldoc--cache-get (ticket)
  "Get TICKET description from cache if valid."
  (when-let* ((entry (gethash ticket jira-eldoc-cache))
              (timestamp (car entry))
              (description (cdr entry)))
    (if (< (- (float-time) timestamp) jira-eldoc-cache-ttl)
        description
      (remhash ticket jira-eldoc-cache)
      nil)))

(defun jira-eldoc--cache-put (ticket description)
  "Store TICKET DESCRIPTION in cache with current timestamp."
  (puthash ticket (cons (float-time) description) jira-eldoc-cache))

(defun jira-eldoc--clear-cache ()
  "Clear the entire Jira eldoc cache."
  (interactive)
  (clrhash jira-eldoc-cache)
  (message "Jira eldoc cache cleared"))

(defun jira-eldoc--fetch-description (ticket)
  "Fetch description for TICKET, using cache when available."
  (or (jira-eldoc--cache-get ticket)
      (when-let ((summary (condition-case nil
                              (jira-summary ticket)
                            (error nil))))
        (jira-eldoc--cache-put ticket summary)
        summary)))

(defun jira-eldoc--ticket-at-point ()
  "Return Jira ticket at point if present, nil otherwise."
  (when-let* ((ticket-pattern "\\b[A-Z]\\{2,10\\}-[0-9]+\\b")
              (thing (thing-at-point 'symbol t)))
    (when (string-match-p (concat "\\`" ticket-pattern "\\'") thing)
      thing)))

;;;###autoload
(defun jira-eldoc-function (callback &rest _ignored)
  "Eldoc documentation function for Jira tickets.
Calls CALLBACK with the ticket description when point is on a Jira ticket.
Designed to work with eldoc-documentation-functions."
  (when-let* ((ticket (jira-eldoc--ticket-at-point))
              ;; Only fetch if in normal state (not while typing)
              (_ (and (bound-and-true-p evil-mode)
                      (eq evil-state 'normal))))
    ;; Fetch asynchronously to avoid blocking
    (run-with-idle-timer
     0.1 nil
     (lambda ()
       (when-let ((description (jira-eldoc--fetch-description ticket)))
         (funcall callback
                  (format "%s: %s" ticket description)
                  :thing ticket
                  :face 'font-lock-doc-face))))))

;;;###autoload
(define-minor-mode jira-eldoc-mode
  "Minor mode to show Jira ticket descriptions in eldoc."
  :global nil
  :lighter nil
  (if jira-eldoc-mode
      (add-hook 'eldoc-documentation-functions #'jira-eldoc-function nil t)
    (remove-hook 'eldoc-documentation-functions #'jira-eldoc-function t)))

;;;###autoload
(defun jira-eldoc-enable ()
  "Enable jira-eldoc-mode in current buffer."
  (interactive)
  (jira-eldoc-mode 1)
  (eldoc-mode 1))

(provide 'jira-eldoc)
;;; jira-eldoc.el ends here
