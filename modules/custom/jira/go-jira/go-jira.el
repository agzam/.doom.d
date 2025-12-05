;;; go-jira.el --- Emacs interface to go-jira CLI tool -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (consult "1.0") (s "1.13.1"))
;; Keywords: tools, jira
;; URL: https://github.com/agzam/go-jira.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an Emacs interface to the go-jira CLI tool
;; (https://github.com/go-jira/jira), allowing you to interact with
;; Jira issues directly from Emacs.
;;
;; Features:
;; - Search and browse Jira issues
;; - View issue details
;; - Convert issue keys to org-mode/markdown links
;; - Generate git branch names from issues
;; - Integration with consult for fuzzy searching

;;; Code:

(require 'json)
(require 'consult)
(require 'thingatpt)
(require 'ansi-color)
(require 'markdown-mode)
(require 's)

(defgroup go-jira nil
  "Emacs interface to go-jira CLI tool."
  :group 'tools
  :prefix "go-jira-")

(defcustom go-jira-default-search-format-string "text ~ \"%s\""
  "Default, initial format string for search."
  :type 'string
  :group 'go-jira)

;;; Internal utilities

(defun go-jira--find-exe (&optional exe)
  "Find and return executable EXE or throw an error.
Defaults to \"jira\" if EXE is not provided."
  (if-let ((ex (executable-find (or exe "jira"))))
      ex
    (error "ERROR: Could not locate %s" (or exe "jira"))))

(defun go-jira--ticket-arg-or-ticket-at-point (&optional ticket)
  "Resolve TICKET based on argument or `symbol-at-point'.
If TICKET is provided, return it.
Otherwise, check for a ticket at point.
Falls back to last kill in `kill-ring' if it's a valid ticket."
  (let* ((ticket-pattern "\\`[A-Z]\\{2,10\\}-[0-9]+\\'")
         (satp (thing-at-point 'symbol t))
         (ticket-at-point (when (and satp
                                     (string-match-p ticket-pattern satp))
                            satp))
         (kill-ring-ticket (when (and (not ticket)
                                      (not ticket-at-point)
                                      kill-ring)
                             (let ((last-kill (car kill-ring)))
                               (when (string-match-p ticket-pattern last-kill)
                                 last-kill)))))
    (or ticket ticket-at-point kill-ring-ticket)))

(defun go-jira--url (ticket)
  "Get the browsable URL for TICKET."
  (let* ((j (go-jira--find-exe))
         (jq (go-jira--find-exe "jq"))
         (cmd (format (concat
                       "%s view %s --template json | %s -r '"
                       "\"\\( .self | split(\"/rest\")[0] )/browse/\\( .key )\"'")
                      j ticket jq)))
    (string-trim (shell-command-to-string cmd))))

(defun go-jira--summary+url (ticket)
  "Fetch summary and url for a given TICKET.
Returns a plist with :ticket, :url, and :summary."
  (let* ((j (go-jira--find-exe))
         (jq (go-jira--find-exe "jq"))
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
             (url (gethash 'url parsed)))
        (list :ticket ticket :url url :summary summary)))))

;;; Public API - Ticket information

(defun go-jira-summary (ticket)
  "Retrieve the summary of TICKET number."
  (interactive)
  (let ((j (go-jira--find-exe)))
    (string-trim
     (shell-command-to-string
      (format "%s view %s --gjq 'fields.summary'" j ticket)))))

;;;###autoload
(defun go-jira-ticket->url (ticket)
  "Extract browsable url for the TICKET number."
  (let* ((j (go-jira--find-exe))
         (jq (go-jira--find-exe "jq"))
         (cmd (format "%s view %s --template json | %s -r '\"\\(.self | split(\"/rest\")[0])/browse/\\(.key)\"'"
                      j ticket jq))
         (res (shell-command-to-string cmd)))
    (if (string-match-p "jq: .* error:" res)
        (user-error res)
      (string-trim res))))

;;;###autoload
(defun go-jira-ticket->link (&optional ticket-arg)
  "Convert the TICKET-ARG number at point to 'org-mode' link."
  (interactive)
  (let* ((ticket (go-jira--ticket-arg-or-ticket-at-point ticket-arg))
         (sum+url (go-jira--summary+url ticket))
         (ticket (plist-get sum+url :ticket))
         (url (plist-get sum+url :url))
         (summary (plist-get sum+url :summary))
         (result (if (eq major-mode 'org-mode)
                     (format "[[%s][%s: %s]]" url ticket summary)
                   (format "[%s: %s](%s)" ticket summary url))))
    (if ticket-arg
        result
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (delete-region (car bounds) (cdr bounds))
        (insert result)))))

;;;###autoload
(defun go-jira-ticket->num+description (&optional ticket-arg)
  "Convert the TICKET-ARG to number and description.
e.g., XYZ-1234 becomes XYZ-1234 - This ticket does nothing"
  (interactive)
  (let* ((ticket-regex "\\b[A-Z]+-[0-9]+\\b")
         (ticket (go-jira--ticket-arg-or-ticket-at-point ticket-arg))
         (already-desc-p (unless ticket-arg
                           (save-excursion
                             (beginning-of-thing 'symbol)
                             (looking-at-p (concat ticket-regex " - '.*'")))))
         (sum+url (go-jira--summary+url ticket))
         (summary (plist-get sum+url :summary))
         (result (format "%s - '%s'" ticket summary)))
    (if ticket-arg
        result
      (unless already-desc-p
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (delete-region (car bounds) (cdr bounds))
          (insert result))))))

;;;###autoload
(defun go-jira-ticket->git-branch-name (&optional ticket-arg)
  "Convert TICKET-ARG to a git branch name.
e.g., SAC-28812 with Add New Metadata to tap-asana
becomes SAC-28812__add_new_metadata_tap-asana"
  (interactive)
  (let* ((ticket (go-jira--ticket-arg-or-ticket-at-point ticket-arg))
         (sum+url (go-jira--summary+url ticket))
         (summary (plist-get sum+url :summary))
         ;; Clean and format the summary
         (clean-summary
          (replace-regexp-in-string
           "_+" "_"                    ; Collapse multiple underscores
           (replace-regexp-in-string
            "^_\\|_$" ""               ; Remove leading/trailing underscores
            (replace-regexp-in-string
             "[^a-z0-9-]+" "_"         ; Replace non-alphanumeric (except hyphen) with underscore
             (replace-regexp-in-string
              "\\b\\(the\\|and\\|or\\|of\\|to\\|in\\|for\\|a\\|an\\|is\\|are\\|was\\|were\\|be\\|been\\|with\\|from\\|at\\|by\\|on\\)\\b" ""
              (downcase summary))))))
         (branch-name (format "%s__%s" ticket clean-summary)))
    ;; Truncate if too long (keep ticket number intact)
    (when (< 80 (length branch-name))
      (setq branch-name (concat (substring branch-name 0 77) "...")))
    (kill-new branch-name)
    (message "Branch name copied: '%s'" branch-name)
    branch-name))

;;; Ticket viewing and browsing

(defvar go-jira-browse-ticket-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'go-jira--browse-ticket-mode-open-browser)
    (define-key map (kbd "q") #'kill-buffer-and-window)
    map)
  "Keymap for `go-jira-browse-ticket-mode' minor mode.")

(define-minor-mode go-jira-browse-ticket-mode
  "Minor mode for buffer with jira ticket content."
  :group 'go-jira
  :lighter " jira"
  :init-value nil
  :keymap go-jira-browse-ticket-mode-map)

(defun go-jira--browse-ticket-mode-open-browser ()
  "Open ticket in browser from browse ticket mode."
  (interactive)
  (let ((ticket
         (buffer-local-value
          'go-jira--ticket-number (current-buffer))))
    (go-jira-browse-ticket-url ticket)))

(defun go-jira--browser-ticket-mode-get-url ()
  "Get URL for ticket in current buffer."
  (interactive)
  (let ((ticket (buffer-local-value 'go-jira--ticket-number (current-buffer))))
    (kill-new (go-jira--url ticket))))

;;;###autoload
(defun go-jira-view-ticket (ticket)
  "View the TICKET in a buffer."
  (interactive "sJira ticket number: ")
  (let* ((j (go-jira--find-exe))
         (buf (get-buffer-create (format "%s" ticket)))
         (cmd (format "%s view %s" j ticket))
         (output (ansi-color-apply (shell-command-to-string cmd)))
         (subtasks-out (thread-last
                         ticket
                         (format "%s list --query 'parent = %s'" j)
                         shell-command-to-string ansi-color-apply))
         (linked-items (thread-last
                         ticket
                         (format "%s list --query 'issue in linkedIssues(%s)'" j)
                         shell-command-to-string ansi-color-apply)))
    (with-current-buffer buf
      (erase-buffer)
      (put 'go-jira--ticket-number 'permanent-local t)
      (setq-local go-jira--ticket-number ticket)
      (insert (replace-regexp-in-string "\r" "" output))
      (unless (s-blank-p subtasks-out)
        (insert "Subtasks:\n")
        (insert subtasks-out))
      (unless (s-blank-p linked-items)
        (insert "Linked work items:\n")
        (insert linked-items))
      (markdown-mode)
      (go-jira-browse-ticket-mode)
      (goto-char (point-min)))
    (display-buffer buf)
    (select-window (get-buffer-window buf))))


;;;###autoload
(defun go-jira-search (&optional query)
  "Search Jira issues using QUERY.
If QUERY is not provided, uses `go-jira-default-search-format-string'."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        ;; place cursor between the quotes
        (search-backward "\""))
    (consult--read
     (consult--async-pipeline
      (consult--async-throttle)
      (consult--async-process
       (lambda (input)
         (when (not (string-match-p "\"\"" input)) ; query has no empty quote blocks
           (list "jira" "list" "--query" input)))))
     :initial (or query (format go-jira-default-search-format-string ""))
     :sort nil ; records must be of the exact order as the go-jira app output
     :state (lambda (action cand)
              (when (and cand (member action '(preview return)))
                (when-let ((ticket (progn (string-match "^[^:]+" cand)
                                          (match-string 0 cand))))
                  (go-jira-view-ticket ticket)))))))

(provide 'go-jira)
;;; go-jira.el ends here
