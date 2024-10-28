;;; go-jira.el --- go-jira helper -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 2024
;; Version: 0.0.1
;; Keywords: tools extensions
;; Homepage: https://github.com/agzam/ag-themes.el
;; Package-Requires: ((emacs "29"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;  Helper functions to work with go-jira: https://github.com/go-jira/jira
;;; Code:

(defun jira-summary (ticket)
  "Retrieves the summary of TICKET number."
  (interactive)
  (let ((j (executable-find "jira")))
    (string-trim
     (shell-command-to-string
      (format "%s view %s --gjq 'fields.summary'" j ticket)))))

(defun jira-ticket->url (ticket)
  "Extracts browsable url for the TICKET number."
  (let* ((j (executable-find "jira"))
         (jq (executable-find "jq"))
         (cmd (format "%s view %s --template json | %s -r '\"\\(.self | split(\"/rest\")[0])/browse/\\(.key)\"'"
                      j ticket jq))
         (res (shell-command-to-string cmd)))
    (if (string-match-p "jq: .* error:" res)
        (user-error res)
      (string-trim res))))


;;;###autoload
(defun jira-ticket->link (&optional ticket)
  "Converts TICKET number at point to org-mode link."
  (interactive)
  (let* ((satp (thing-at-point 'symbol t))
         (ticket-at-point (when (and satp (string-match "\\b[A-Z]+-[0-9]+\\b" satp))
                            satp))
         (_ (unless (string-match-p "^[A-Z]\\{3,5\\}-[0-9]+$" ticket-at-point)
              (user-error "not a ticket number")))
         (ticket (or ticket ticket-at-point))
         (j (executable-find "jira"))
         (jq (executable-find "jq"))
         (cmd (format (concat
                       "%s view %s --template json | %s -r '{"
                       "summary: .fields.summary,"
                       "url: \"\\( .self | split(\"/rest\")[0] )/browse/\\( .key )\"}'")
                      j ticket jq))
         (res (shell-command-to-string cmd)))
    (if (string-match-p "jq: .* error:" res)
        (user-error res)
      (let* ((json-object-type 'hash-table)
             (json-key-type 'symbol)
             (json-array-type 'list)
             (parsed (json-read-from-string res))
             (summary (gethash 'summary parsed))
             (url (gethash 'url parsed))
             (result (if (eq major-mode 'org-mode)
                         (format "[[%s][%s: %s]]" url ticket summary)
                       (format "[%s: %s](%s)" ticket summary url))))
        (if ticket-at-point
            (let ((bounds (bounds-of-thing-at-point 'symbol)))
              (delete-region (car bounds) (cdr bounds))
              (insert result))
          result)))))


(defun jira-view-simple (ticket)
  "View the TICKET in a buffer."
  (interactive "sJira ticket number: ")
  (let* ((j (executable-find "jira"))
         (buf (get-buffer-create (format "%s" ticket)))
         (cmd (format "%s view %s" j ticket))
         (output (ansi-color-apply (shell-command-to-string cmd))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (replace-regexp-in-string "\r" "" output))
      (markdown-mode)
      (goto-char (point-min)))
    (display-buffer buf)
    (select-window (get-buffer-window buf)))
  ;; TODO: C-c C-o to browse
  )


(defun jira-search (query)
  "Search through tickets."
  )

;;; go-jira.el ends here
