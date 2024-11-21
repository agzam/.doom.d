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

(defcustom jira-default-search-format-string "text ~ \"%s\""
  "Default, initial format string for search."
  :type 'string
  :group 'jira)

(defvar jira-browse-ticket-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'jira-browse-ticket-url)
    (define-key map (kbd "q") #'kill-buffer-and-window)
    map)
  "Keymap for `jira-browse-ticket-mode' minor mode. ")

(define-minor-mode jira-browse-ticket-mode
  "Minor mode for buffer with jira ticket content."
  :group 'jira
  :lighter " jira"
  :init-value nil
  :keymap jira-browse-ticket-mode-map)

(defun jira--find-exe (&optional exe)
  "Find and return executable EXE or throw a message."
  (if-let ((ex (executable-find (or exe "jira"))))
      ex
    (error "ERROR: Could not locate %s" exe)))

(defun jira-summary (ticket)
  "Retrieves the summary of TICKET number."
  (interactive)
  (let ((j (jira--find-exe)))
    (string-trim
     (shell-command-to-string
      (format "%s view %s --gjq 'fields.summary'" j ticket)))))

(defun jira-ticket->url (ticket)
  "Extracts browsable url for the TICKET number."
  (let* ((j (jira--find-exe))
         (jq (jira--find-exe "jq"))
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
         (j (jira--find-exe))
         (jq (jira--find-exe "jq"))
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

;;;###autoload
(defun jira-view-simple (ticket)
  "View the TICKET in a buffer."
  (interactive "sJira ticket number: ")
  (let* ((j (jira--find-exe))
         (buf (get-buffer-create (format "%s" ticket)))
         (cmd (format "%s view %s" j ticket))
         (output (ansi-color-apply (shell-command-to-string cmd))))
    (with-current-buffer buf
      (erase-buffer)
      (put 'jira--ticket-number 'permanent-local t)
      (setq-local jira--ticket-number ticket)
      (insert (replace-regexp-in-string "\r" "" output))
      (markdown-mode)
      (jira-browse-ticket-mode)
      (goto-char (point-min)))
    (display-buffer buf)
    (select-window (get-buffer-window buf))))

(defun jira-browse-ticket-url (&optional ticket)
  (interactive)
  (let ((j (jira--find-exe))
        (ticket (or ticket
                    (buffer-local-value 'jira--ticket-number (current-buffer)))))
    (shell-command-to-string (format "%s browse %s" j ticket))))

;;;###autoload
(defun jira-search (&optional query)
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        ;; place cursor between the quotes
        (search-backward "\""))
    (consult--read
     (consult--async-command
         (lambda (input)
           (when (not (string-match-p "\"\"" input)) ; query has no empty quote blocks
             (list "jira" "list" "--query" input)))
       :throttle 0.5)
     :initial (or query (format jira-default-search-format-string ""))
     :state (lambda (action cand)
              (when (and cand (member action '(preview return)))
                (when-let ((ticket (progn (string-match "^[^:]+" cand)
                                          (match-string 0 cand))))
                  (jira-view-simple ticket)))))))


;;; go-jira.el ends here
