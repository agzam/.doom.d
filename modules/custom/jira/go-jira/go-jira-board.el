;;; go-jira-board.el --- Jira board browsing for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions for browsing and displaying Jira boards.
;; Provides consult-based board selection and configuration retrieval.

;;; Code:

(require 'json)
(require 'consult)
(require 'go-jira)

(defcustom go-jira-default-project "SAC"
  "Default Jira project key for board browsing."
  :type 'string
  :group 'go-jira)

(defcustom go-jira-board-show-active-sprint-only t
  "When non-nil, show only issues in the active sprint.
When nil, show all issues matching the board's filter."
  :type 'boolean
  :group 'go-jira)

;;; Internal board API functions

(defun go-jira--fetch-boards (project)
  "Fetch all boards for PROJECT.
Returns a list of plists with :id, :name, :type, :project keys."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/agile/1.0/board?projectKeyOrId=%s" project))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (boards (gethash 'values parsed)))
          (unless boards
            (error "No boards found for project: %s" project))
          (mapcar
           (lambda (board)
             (list :id (gethash 'id board)
                   :name (gethash 'name board)
                   :type (gethash 'type board)
                   :project (gethash 'projectKey (gethash 'location board))))
           boards))
      (error
       (error "Failed to fetch boards: %s\nOutput: %s" (error-message-string err) output)))))

(defun go-jira--fetch-board-config (board-id)
  "Fetch configuration for BOARD-ID.
Returns a plist with :filter-id and :columns keys."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/agile/1.0/board/%d/configuration" board-id))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (filter (gethash 'filter parsed))
               (filter-id (when filter (gethash 'id filter)))
               (column-config (gethash 'columnConfig parsed))
               (columns (when column-config (gethash 'columns column-config))))
          (list :filter-id filter-id
                :columns (go-jira--parse-board-columns columns)))
      (error
       (error "Failed to fetch board config for board %d: %s" board-id (error-message-string err))))))

(defun go-jira--parse-board-columns (columns)
  "Parse COLUMNS from board configuration API response.
Returns an alist of (column-name . (status-id-list))."
  (mapcar
   (lambda (col)
     (let ((name (gethash 'name col))
           (statuses (gethash 'statuses col)))
       (cons name
             (mapcar (lambda (status) (gethash 'id status))
                     statuses))))
   columns))

(defun go-jira--fetch-filter-jql (filter-id)
  "Fetch JQL query string for FILTER-ID."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/api/2/filter/%s" filter-id))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (jql (gethash 'jql parsed)))
          (unless jql
            (error "No JQL found for filter: %s" filter-id))
          jql)
      (error
       (error "Failed to fetch filter JQL for filter %s: %s" filter-id (error-message-string err))))))

(defun go-jira--fetch-active-sprint (board-id)
  "Fetch active sprint ID for BOARD-ID.
Returns the sprint ID or nil if no active sprint."
  (let* ((j (go-jira--find-exe))
         (endpoint (format "/rest/agile/1.0/board/%d/sprint?state=active" board-id))
         (cmd (format "%s request '%s' --method GET" j endpoint))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (sprints (gethash 'values parsed)))
          (when (and sprints (> (length sprints) 0))
            (gethash 'id (car sprints))))
      (error
       (message "Warning: Could not fetch active sprint: %s" (error-message-string err))
       nil))))

(defun go-jira--board-candidate (board)
  "Create a consult candidate from BOARD plist.
Returns a propertized string with board data attached."
  (let* ((name (plist-get board :name))
         (type (plist-get board :type))
         (project (plist-get board :project))
         (annotation (format " [%s] (%s)" type project)))
    (propertize name
                'board-data board
                'consult--candidate name
                'consult--annotation annotation)))

(defun go-jira--get-board-data (board-id name type project)
  "Fetch complete board data for BOARD-ID.
Returns a plist with all board information including JQL and columns."
  (message "Fetching board configuration...")
  (let* ((config (go-jira--fetch-board-config board-id))
         (filter-id (plist-get config :filter-id))
         (columns (plist-get config :columns))
         (jql (when filter-id
                (message "Fetching filter query...")
                (go-jira--fetch-filter-jql filter-id)))
         (active-sprint-id (progn
                             (message "Fetching active sprint...")
                             (go-jira--fetch-active-sprint board-id))))
    (message "Board data retrieved")
    (list :id board-id
          :name name
          :type type
          :project project
          :filter-id filter-id
          :jql jql
          :columns columns
          :active-sprint-id active-sprint-id)))

;;; Issue fetching and board display

(defun go-jira--fetch-issues-by-jql (jql &optional limit)
  "Fetch issues matching JQL query.
Returns a list of issue plists with :key, :summary, :status, etc.
Optional LIMIT restricts number of results (default: no limit)."
  (let* ((j (go-jira--find-exe))
         (limit-arg (if limit (format " --limit %d" limit) ""))
         (cmd (format "%s list --query '%s' --queryfields 'key,summary,status,assignee,priority,labels,issuetype' --template json%s"
                      j jql limit-arg))
         (output (shell-command-to-string cmd)))
    (condition-case err
        (let* ((json-object-type 'hash-table)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (parsed (json-read-from-string output))
               (issues (gethash 'issues parsed)))
          (unless issues
            (error "No issues found for JQL query"))
          (mapcar #'go-jira--parse-issue issues))
      (error
       (error "Failed to fetch issues: %s\nOutput: %s" (error-message-string err) output)))))

(defun go-jira--parse-issue (issue-json)
  "Parse ISSUE-JSON hash-table into a plist."
  (let* ((key (gethash 'key issue-json))
         (fields (gethash 'fields issue-json))
         (summary (gethash 'summary fields))
         (status (gethash 'status fields))
         (status-id (when status (gethash 'id status)))
         (status-name (when status (gethash 'name status)))
         (assignee (gethash 'assignee fields))
         (assignee-name (when assignee (gethash 'displayName assignee)))
         (priority (gethash 'priority fields))
         (priority-name (when priority (gethash 'name priority)))
         (issuetype (gethash 'issuetype fields))
         (issuetype-name (when issuetype (gethash 'name issuetype)))
         (labels (gethash 'labels fields)))
    (list :key key
          :summary summary
          :status-id status-id
          :status-name status-name
          :assignee assignee-name
          :priority priority-name
          :issuetype issuetype-name
          :labels (when labels (mapcar #'identity labels)))))

(defun go-jira--group-issues-by-column (issues columns)
  "Group ISSUES by board COLUMNS using status IDs.
Returns an alist of (column-name . (issue-list))."
  (let ((grouped '()))
    (dolist (col columns)
      (let* ((col-name (car col))
             (status-ids (cdr col))
             (col-issues (seq-filter
                          (lambda (issue)
                            (member (plist-get issue :status-id) status-ids))
                          issues)))
        (push (cons col-name col-issues) grouped)))
    (nreverse grouped)))

(defun go-jira--build-board-buffer (board-data issues)
  "Build and return org-mode buffer for BOARD-DATA with ISSUES."
  (let* ((board-name (plist-get board-data :name))
         (columns (plist-get board-data :columns))
         (grouped (go-jira--group-issues-by-column issues columns))
         (buf-name (format "*Jira Board: %s*" board-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      
      ;; Insert org-columns setup
      (insert "#+COLUMNS: %50ITEM %12TODO %15ASSIGNEE %12PRIORITY %10ISSUETYPE %25LABELS\n")
      (insert "#+TITLE: " board-name "\n\n")
      
      ;; Insert each column with its issues
      (dolist (col-group grouped)
        (let ((col-name (car col-group))
              (col-issues (cdr col-group)))
          (insert "* " col-name "\n")
          (when col-issues
            (dolist (issue col-issues)
              (go-jira--insert-issue issue)))))
      
      ;; Enable board view mode
      (go-jira-board-view-mode)
      (setq-local go-jira--board-data board-data)
      (goto-char (point-min))
      ;; Show all headings (level 1 and 2) but hide property drawers
      (org-content 2))
    buf))

(defun go-jira--insert-issue (issue)
  "Insert ISSUE as org heading with properties."
  (let ((key (plist-get issue :key))
        (summary (plist-get issue :summary))
        (status (or (plist-get issue :status-name) ""))
        (assignee (or (plist-get issue :assignee) "Unassigned"))
        (priority (or (plist-get issue :priority) ""))
        (issuetype (or (plist-get issue :issuetype) ""))
        (labels (plist-get issue :labels)))
    (insert "** " key ": " summary "\n")
    (insert ":PROPERTIES:\n")
    (insert ":ISSUE_KEY: " key "\n")
    (insert ":TODO: " status "\n")
    (insert ":ASSIGNEE: " assignee "\n")
    (insert ":PRIORITY: " priority "\n")
    (insert ":ISSUETYPE: " issuetype "\n")
    (when labels
      (insert ":LABELS: " (mapconcat #'identity labels ", ") "\n"))
    (insert ":END:\n")))

;;; Board view mode

(defvar go-jira--board-data nil
  "Buffer-local variable storing board data plist.")

(defvar go-jira-board-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'go-jira-board-view-issue)
    (define-key map (kbd "b") #'go-jira-board-browse-issue-url)
    (define-key map (kbd "r") #'go-jira-board-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-c C-c") #'org-columns)
    map)
  "Keymap for `go-jira-board-view-mode'.")

(define-derived-mode go-jira-board-view-mode org-mode "Jira-Board"
  "Major mode for viewing Jira boards in org-mode format.
\\{go-jira-board-view-mode-map}"
  :group 'go-jira
  (setq-local buffer-read-only t)
  (setq-local org-startup-folded 'overview)
  (message "Press 'C-c C-c' to toggle columns view, 'RET' to view issue, 'b' to browse in browser, 'r' to refresh, 'q' to quit"))

(defun go-jira-board-view-issue ()
  "View the Jira issue at point."
  (interactive)
  (when-let* ((props (org-entry-properties))
              (key (cdr (assoc "ISSUE_KEY" props))))
    (go-jira-view-ticket key)))

(defun go-jira-board-browse-issue-url ()
  "Open the Jira issue at point in browser."
  (interactive)
  (when-let* ((props (org-entry-properties))
              (key (cdr (assoc "ISSUE_KEY" props))))
    (browse-url (go-jira-ticket->url key))))

(defun go-jira-board-refresh ()
  "Refresh the current board view."
  (interactive)
  (if-let ((board-data (buffer-local-value 'go-jira--board-data (current-buffer))))
      (progn
        (message "Refreshing board...")
        (go-jira-display-board board-data))
    (user-error "No board data found in current buffer")))

;;; Public API

;;;###autoload
(defun go-jira-browse-boards (&optional project display)
  "Browse and select a Jira board.
With prefix arg, prompt for PROJECT. Otherwise use
`go-jira-default-project'.

When called interactively, automatically displays the board.
When called from Lisp, returns a plist with complete board data
including JQL query and column mappings. Pass DISPLAY non-nil
to display the board immediately."
  (interactive
   (list (when current-prefix-arg
           (read-string "Project: " go-jira-default-project))
         t))
  (let* ((project (or project go-jira-default-project))
         (_ (message "Fetching boards for project %s..." project))
         (boards (go-jira--fetch-boards project))
         ;; Create lookup table: board-name -> board-data
         (board-table (make-hash-table :test 'equal))
         (candidates (mapcar (lambda (board)
                               (let* ((name (plist-get board :name))
                                      (type (plist-get board :type))
                                      (project (plist-get board :project))
                                      (display-name (format "%s [%s] (%s)" name type project)))
                                 (puthash display-name board board-table)
                                 display-name))
                             boards))
         (selected-str (consult--read
                        candidates
                        :prompt (format "Board [%s]: " project)
                        :sort nil
                        :require-match t
                        :category 'jira-board
                        :history 'jira-board-history))
         (selected (when selected-str
                     (gethash selected-str board-table))))
    (when selected
      (let ((board-data (go-jira--get-board-data
                         (plist-get selected :id)
                         (plist-get selected :name)
                         (plist-get selected :type)
                         (plist-get selected :project))))
        (when display
          (go-jira-display-board board-data))
        board-data))))

;;;###autoload
(defun go-jira-display-board (&optional board-data)
  "Display a Jira board in org-mode format.
If BOARD-DATA is not provided, prompts for board selection via
`go-jira-browse-boards'. BOARD-DATA should be a plist with :jql
and :columns keys.

When `go-jira-board-show-active-sprint-only' is non-nil (default),
only shows issues in the active sprint."
  (interactive)
  (let ((board-data (or board-data (go-jira-browse-boards))))
    (unless board-data
      (user-error "No board selected"))
    (let* ((base-jql (plist-get board-data :jql))
           (active-sprint-id (plist-get board-data :active-sprint-id))
           ;; Extract ORDER BY clause if present
           (order-by-regex "\\s-+ORDER\\s-+BY\\s-+.+$")
           (has-order-by (string-match-p order-by-regex base-jql))
           (jql-without-order (if has-order-by
                                  (replace-regexp-in-string order-by-regex "" base-jql)
                                base-jql))
           (order-by-clause (when has-order-by
                              (string-match order-by-regex base-jql)
                              (match-string 0 base-jql)))
           (jql (if (and go-jira-board-show-active-sprint-only active-sprint-id)
                    (format "(%s) AND Sprint = %d%s"
                            jql-without-order
                            active-sprint-id
                            (or order-by-clause ""))
                  base-jql)))
      (unless base-jql
        (user-error "No JQL query found for board: %s" (plist-get board-data :name)))
      (when (and go-jira-board-show-active-sprint-only (not active-sprint-id))
        (message "Warning: No active sprint found, showing all issues"))
      (message "Fetching issues for board: %s..." (plist-get board-data :name))
      (let* ((issues (go-jira--fetch-issues-by-jql jql))
             (buf (go-jira--build-board-buffer board-data issues)))
        (pop-to-buffer buf)
        (message "Loaded %d issues. Press 'C-c C-c' for columns view." (length issues))))))

(provide 'go-jira-board)
;;; go-jira-board.el ends here
