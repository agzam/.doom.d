;;; custom/org/autoload/custom.el -*- lexical-binding: t; -*-

;;;###autoload
(defun org-roam--link-to (node-title-or-id)
  "For a given NODE-TITLE-OR-ID tries to find the node and returns
Org-link text to the node."
  (when-let* ((nodes (org-roam-db-query
                      "select n.id, n.title from nodes n
                       left join aliases a
                       on n.id = a.node_id
                       where n.id = $s1
                         or title = $s1 collate nocase
                         or a.alias = $s1 collate nocase"
                      node-title-or-id))
              (node (cl-first nodes)))
    (apply 'format "[[id:%s][%s]]" node)))

;;;###autoload
(defun org--insert-selection-dwim (selection)
  "Insert SELECTION as an org blockquote."
  (unless (string= selection "")
    (format "#+begin_quote\n%s\n#+end_quote" selection)))

;;;###autoload
(defun org-roam-capture-dailies--set-node-props (node-link)
  "It's to be used with org-roam-capture and specifically for
dailies. When called from a capture template it inserts title and
id, then finds the datetree entry and inserts link to a
NODE-LINK  - which is title or id or a node."
  (when-let* ((buf (org-capture-get :buffer)))
    (with-current-buffer buf
      (save-excursion
        (forward-line 1)
        (unless (org-entry-get (point) "ID")
          (org-roam-add-property (org-id-new) "ID"))
        (unless (search-forward "#+title:" nil :no-error)
          (forward-line 1)
          (search-forward ":END:" nil :no-error)
          (forward-line)
          (insert (format "#+title: %s %s notes\n"
                          (format-time-string "%B %Y")
                          (org-capture-get :description)))))
      (save-excursion
        (pcase-let* ((`(,sec ,min ,hr ,day ,month ,year) (decode-time (org-capture-get :default-time)))
                     (date (list month day year))
                     (link (org-roam--link-to node-link))
                     (_ (org-datetree-find-date-create date :keep-restriction))
                     (children? (unless (org-capture-get :new-buffer)
                                  (< 0 (1- (length (save-excursion
                                                     (org-map-entries nil nil 'tree)))))))
                     (level (+ (org-current-level) 1)))
          (unless children?
            (save-excursion
             (org-end-of-meta-data)
             (insert link)
             (insert "\n")))
          (let ((tree-limit (save-excursion (org-end-of-subtree) (point))))
            (plist-put org-capture-plist :exact-position tree-limit)
            (concat (make-string level ?*) " ")))))))

;;;###autoload
(cl-defun org-roam-node-insert+ (&optional lines-before lines-after &key templates info)
  "Improved org-roam-node-insert that additionally also removes conflicting and
duplicating links around the context.  If a node has 'collides_with:' property,
inserting a link to that node would remove any links to nodes with IDs contained
in that prop."
  (interactive)
  (unwind-protect
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node-to-insert (org-roam-node-read region-text))   ; first we choose a node to insert
               (description (or region-text (org-roam-node-title node-to-insert)))
               ;; then we find the IDs of nodes, links that need to be removed if that link to be inserted
               (id+collides (seq-remove
                             'string-empty-p
                             (-> node-to-insert
                                 (org-roam-node-properties)
                                 (a-get "COLLIDES_WITH")
                                 (concat " " (org-roam-node-id node-to-insert))
                                 (split-string " "))))
               (re (concat "\\(\\[\\[\\)\\(id:\\|roam:\\)"
                           "\\(" (mapconcat 'identity id+collides "\\|")
                           "\\)\\]\\[\\w*\\]\\]"))
               (before (or lines-before 0))
               (after (or lines-after 0)))
          (save-mark-and-excursion
            (unless region-text
              (setq prev-pos (point))
              (previous-line before)
              (beginning-of-line)
              (set-mark (point))
              (next-line (+ before after))
              (end-of-line))
            (save-restriction
              (narrow-to-region (region-beginning) (region-end))
              (if (org-roam-node-id node-to-insert)
                  (progn
                    (when region-text
                      (delete-region beg end)
                      (set-marker beg nil)
                      (set-marker end nil))
                    (unless region-text
                      (goto-char 0)
                      (while (re-search-forward re nil :no-error)
                        (replace-match ""))
                      (goto-char prev-pos))
                    (makunbound 'prev-pos)
                    (insert (org-link-make-string
                             (concat "id:" (org-roam-node-id node-to-insert))
                             description)))
                (org-roam-capture-
                 :node node-to-insert
                 :info info
                 :templates templates
                 :props (append
                         (when (and beg end)
                           (list :region (cons beg end)))
                         (list :insert-at (point-marker)
                               :link-description description
                               :finalize 'insert-link
                               :immediate-finish t
                               :jump-to-captured nil))))
              ;; make sure links always separated by a single space
              (goto-char 0)
              (while (re-search-forward "\\]\\]\\[\\[" nil :no-error)
                (replace-match "]] [["))))))))

;;;###autoload
(defun org-roam-toggle-ui-xwidget ()
  (interactive)
  (let ((url (concat "http://localhost:" (number-to-string org-roam-ui-port))))
    (if-let ((buf (xwidget-webkit-get-url-buffer
                   (concat "localhost:" (number-to-string org-roam-ui-port)))))
        (if-let ((win (get-buffer-window buf)))
            (delete-window win)
          (switch-to-buffer-other-window buf))
      (xwidget-webkit-url-get-create url "*org-roam-ui*"))))
