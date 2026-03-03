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
(defun vulpea-insert+ (&optional lines-before lines-after)
  "Insert a link to a vulpea note, removing conflicting links.
If the selected note has a `collides_with' property, any existing links
to nodes with IDs listed in that property are removed from the surrounding
context (defined by LINES-BEFORE and LINES-AFTER, defaulting to 0).
Also ensures adjacent links are separated by a space."
  (interactive)
  (atomic-change-group
    (let* (region-text
           beg end
           (_ (when (region-active-p)
                (setq beg (set-marker (make-marker) (region-beginning)))
                (setq end (set-marker (make-marker) (region-end)))
                (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
           (note (vulpea-select "Note" :initial-prompt region-text))
           (description (or region-text (vulpea-note-title note)))
           ;; Find IDs of nodes whose links should be removed
           (id+collides (seq-remove
                         'string-empty-p
                         (split-string
                          (concat (or (cdr (assoc "COLLIDES_WITH" (vulpea-note-properties note))) "")
                                  " " (or (vulpea-note-id note) ""))
                          " ")))
           ;; Matches [[id:ID][any description]] and [[id:ID]] (no description)
           (re (concat "\\[\\[\\(id:\\|roam:\\)"
                       "\\(" (mapconcat 'identity id+collides "\\|")
                       "\\)\\]\\(?:\\[[^]]*\\]\\)?\\]"))
           (before (or lines-before 0))
           (after (or lines-after 0))
           (insert-pos nil))
      (save-mark-and-excursion
        (unless region-text
          (setq insert-pos (copy-marker (point)))
          (forward-line (- before))
          (beginning-of-line)
          (set-mark (point))
          (forward-line (+ before after))
          (end-of-line))
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (if (vulpea-note-id note)
              (progn
                (when region-text
                  (delete-region beg end)
                  (set-marker beg nil)
                  (set-marker end nil))
                (unless region-text
                  (goto-char (point-min))
                  (while (re-search-forward re nil :no-error)
                    (replace-match ""))
                  (goto-char insert-pos)
                  (set-marker insert-pos nil))
                (insert (org-link-make-string
                         (concat "id:" (vulpea-note-id note))
                         description)))
            ;; New note — create it, then insert link
            (let ((new-note (vulpea-create (vulpea-note-title note))))
              (when region-text
                (delete-region beg end)
                (set-marker beg nil)
                (set-marker end nil))
              (insert (org-link-make-string
                       (concat "id:" (vulpea-note-id new-note))
                       description))))
          ;; Make sure links are always separated by a single space
          (goto-char (point-min))
          (while (re-search-forward "\\]\\]\\[\\[" nil :no-error)
            (replace-match "]] [[")))))))

;;;###autoload
(defun org-roam-toggle-ui-xwidget ()
  (interactive)
  (let* ((url (concat "http://localhost:" (number-to-string org-roam-ui-port)))
         (buf (or (xwidget-webkit-get-url-buffer
                   (concat "localhost:" (number-to-string org-roam-ui-port)))
                  (xwidget-webkit-url-get-create url "*org-roam-ui*"))))
    (if-let ((win (get-buffer-window buf)))
        (delete-window win)
      (switch-to-buffer-other-window buf))))

;;;###autoload
(defun get-gh-item-title (uri)
  "Based on given GitHub URI for pull-request or issue,
  return the title of that pull-request or issue."
  (cond
   (;; either PR or issue
    (string-match "\\(github.com\\).*\\(issues\\|pull\\)" uri)
    (pcase-let*
        ((`(_ _ ,owner ,repo ,type ,number) (remove "" (split-string uri "/")))
         (gh-resource (format "/repos/%s/%s/%s/%s"
                              owner
                              repo
                              (if (string= type "pull") "pulls" type)
                              number))
         (resp (ghub-get gh-resource nil :auth 'forge)))
      (when resp
        (let-alist resp
          (format
           "%s/%s#%s — %s" owner repo number .title)))))

   ;; just a link to a repo
   ((string-match "\\(github.com\\)/[[:graph:]]+/[[:graph:]]+$" uri)
    (pcase-let* ((uri* (->> (split-string uri "/\\|\\?")
                            (remove "")
                            (-non-nil)))
                 (`(_ _ ,owner ,repo) uri*)
                 (gh-resource (format "/repos/%s/%s" owner repo))
                 (resp (ghub-get gh-resource nil :auth 'forge)))
      (when resp
        (let-alist resp
          (format
           "%s/%s — %s" owner repo .description)))))

   (;; link to a file in a branch
    (string-match "\\(github.com\\).*" uri)
    (pcase-let* ((uri* (->> (split-string uri "/\\|\\?")
                            (remove "")
                            (-non-nil)))
                 (`(_ _ ,owner ,repo ,type ,branch ,dir ,file) uri*)
                 (branch (if (or (string= type "commit") (string= type "tree"))
                             (substring branch 0 7)  ; trim to shorten sha
                           branch)))
      (mapconcat
       'identity (->> (list owner repo type branch dir file) (-non-nil))
       "/")))
   (t uri)))

;;;###autoload
(defun +org-link-make-description-function (link desc)
  (cond ((not (s-blank? desc)) desc)
        ((string-match "\\(github.com\\).*" link)
         (get-gh-item-title link))
        (t desc)))

(defun org-link-parse (link)
  ;; borrowed and adopted from:
  ;; github.com/xuchunyang/emacs.d/blob/5f4f873cf7a671a36f686f3d1346fd7c5a5462bc/lisp/chunyang-misc.el#L488-L526
  (if (or (string-match (rx "[[" (group (0+ anything)) "][" (group (0+ anything)) "]]") link)
          (string-match (rx "[[" (group (0+ anything)) "]]") link)
          (string-match (rx  (group (0+ anything))) link))
      (seq-remove #'null
                  (list (match-string 1 link)
                        (match-string 2 link)))
    (error "Cannot parse %s as Org link" link)))


;;;###autoload
(defun org-store-link-id-optional (&optional arg)
  "Stores a link, reversing the value of `org-id-link-to-org-use-id'.
If it's globally set to create the ID property, then it wouldn't,
and if it is set to nil, then it would forcefully create the ID."
  (interactive "P")
  (let ((org-id-link-to-org-use-id (not org-id-link-to-org-use-id)))
    (org-store-link arg :interactive)))

;;;###autoload
(defun edit-indirect-guess-mode-fn+ (parent-buffer beg _end)
  "Guess the major mode for an edit-indirect buffer."
  (let* ((type (with-current-buffer parent-buffer
                 (cond
                  ;; set markdown-mode for quote & verse blocks
                  ((and (eq major-mode 'org-mode)
                        (when-let ((s (save-mark-and-excursion
                                        (goto-char (- beg 1))
                                        (thing-at-point 'symbol))))
                          (string-match-p
                           "+begin_quote\\|+begin_verse" s)))
                   :quote)
                  ((and (eq major-mode 'org-mode)
                        (when-let ((s (save-mark-and-excursion
                                        (goto-char (- beg 1))
                                        (backward-word)
                                        (thing-at-point 'word))))
                          (string-match-p "results" s)))
                   :results-drawer)

                  ((eq major-mode 'org-mode) :org-mode)))))
    (cl-case type
      (:quote (markdown-mode))
      (:results-drawer (json-mode))
      (:org-mode (org-mode))
      (t (normal-mode)))))

;;;###autoload
(defun +org-goto-bottommost-heading (&optional maxlevel)
  "Go to the last heading in the current subtree."
  (interactive "P")
  (if (listp maxlevel)
      (setq maxlevel 4)
    (unless maxlevel (setq maxlevel 3)))
  (setq currlevel 1)
  (while (<= currlevel maxlevel)
    (org-next-visible-heading 1)
    (if (not (org-at-heading-p))
        (progn
          (org-previous-visible-heading 1)
          (org-cycle)
          (setq currlevel (1+ currlevel))))))

;;;###autoload
(defun +org-goto-datetree-date (&optional date)
  "Jump to selected date heading in the datetree."
  (interactive)
  (save-restriction
    (let* ((datetree-date (org-read-date))
           (dt (org-date-to-gregorian datetree-date)))
      (org-datetree-find-date-create dt t)
      (org-show-hidden-entry)
      (show-subtree))))

;;;###autoload
(defun +person-w-name-based-id ()
  "Returns a person record with name-based id. To be used in capture template."
  (interactive)
  (let* ((name-parts (thread-last
                       (or (org-capture-get ':initial)
                           (gui-get-selection 'CLIPBOARD))
                       (read-from-minibuffer "Name: ")
                       (split-string)
                       (seq-map 's-trim)
                       (seq-map 'capitalize)))
         (id (mapconcat 'downcase (reverse name-parts) "-"))
         (name (format "%s %s" (car name-parts)
                       (substring (cadr name-parts) 0 1)))
         (full-name (mapconcat 'identity name-parts " ")))
    (format "%s\n:PROPERTIES:\n:ID: %s\n:roam_aliases: \"%s\"\n:END:" name id full-name)))

;;;###autoload
(defun org-wrap-in-block (block-type)
  "Wrap selected region in an org-mode block of BLOCK-TYPE."
  (interactive
   (list (completing-read "Block type: " '("src" "example" "quote" "center" "verse"))))
  (let ((start (if (region-active-p) (region-beginning)
                 (save-excursion (backward-paragraph) (forward-char) (point))))
        (end (if (region-active-p) (region-end)
               (save-excursion (forward-paragraph) (backward-char) (point))))
        (block-start (format "#+begin_%s" block-type))
        (block-end (format "#+end_%s" block-type)))
    (save-excursion
      (goto-char end)
      (insert "\n" block-end)
      (goto-char start)
      (insert block-start "\n"))
    (when (string= block-type "src")
      (goto-char start)
      (end-of-line)
      (insert " ")
      (evil-insert-state))))
