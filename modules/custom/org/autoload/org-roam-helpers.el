;;; custom/org/autoload/org-roam-helpers.el -*- lexical-binding: t; -*-

(defvar vulpea-backlinks--target-id nil
  "Target note ID from the most recent `vulpea-backlinks' session.
Used by embark actions to show sparse trees in collect buffers.")

;;;###autoload
(defun vulpea-backlinks-sparse-tree (target-id)
  "Show sparse tree revealing all links to TARGET-ID in current buffer.
Folds the buffer, highlights matches, and sets up `next-error'
navigation via M-g M-n / M-g M-p."
  (let ((regexp (format "\\[\\[id:%s\\]\\[[^]]*\\]\\]" (regexp-quote target-id))))
    (org-cycle-overview)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (org-highlight-new-match (match-beginning 0) (match-end 0))
        (org-fold-show-context 'occur-tree)))
    (setq-local next-error-function #'org-occur-next-match)
    (setq next-error-last-buffer (current-buffer))
    (org-fold-show-context 'occur-tree)
    (recenter)))

;;;###autoload
(defun outline-collapsed? ()
  "Returns nil if the top outline heading is collapsed (hidden)"
  (save-excursion
    (when (org-get-outline-path 'with-self?)
      (ignore-errors (outline-up-heading 1))
      (let* ((beg (point))
             (end (+ 1 (line-end-position))))
        (not
         (seq-empty-p
          (seq-filter
           (lambda (o)
             (and (eq (overlay-get o 'invisible) 'outline)
                  (save-excursion
                    (goto-char (overlay-start o))
                    (outline-on-heading-p t))))
           (overlays-in beg end))))))))

;;;###autoload
(defun org--get-headline-with-text ()
  "Grabs top level headline and its content"
  (save-excursion
    (save-restriction
      (ignore-errors (outline-up-heading 1))
      (let ((heading-shown? (not (outline-collapsed?))))
        (when heading-shown? (hide-subtree))
        (let* ((elt (org-element-at-point))
               (title (org-element-property :title elt))
               (beg (progn (org-end-of-meta-data t) (point)))
               (end (progn (outline-next-visible-heading 1) (point))))
          (when heading-shown? (show-subtree))
          (list title (buffer-substring-no-properties beg end)))))))

;;;###autoload
(defun org-roam-refile-to-node ()
  "Refile heading to another Org-roam node."
  (interactive)
  (let* ((headline? (org-get-outline-path 'with-self?))
         (props (org-entry-properties))
         (id (org-entry-get nil "id"))
         (title (if headline?
                    (org-element-property :title (org-element-at-point))
                  (cadar (org-collect-keywords '("title")))))
         (content (cadr (org--get-headline-with-text))))
    (when headline?
      (org-cut-subtree))
    (org-roam-capture-
     :goto nil
     :node (org-roam-node-create
            :title title
            :properties props
            :id id)
     :props '(:immediate-finish nil))
    (let ((insert-at (plist-get org-capture-plist :position-for-last-stored)))
      (with-current-buffer (marker-buffer insert-at)
        (insert content)
        (mark-whole-buffer)
        (org-do-promote)))))

;;;###autoload
(defun org-roam-ui-browser+ ()
  (interactive)
  (browse-url
   (concat "http://localhost:"
           (number-to-string org-roam-ui-port))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-roam-count-overlays  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface org-roam-count-overlay-face
  '((t :inherit shadow :height 1.3 :underline nil :weight light))
  "Face for Org Roam count overlay.")

(defconst org-roam--superscript-digits
  (vector "⁰" "¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹"))

(defun org-roam--number-to-superscript (n)
  "Convert integer N to a string of Unicode superscript digits."
  (mapconcat (lambda (c) (aref org-roam--superscript-digits (- c ?0)))
             (number-to-string n) ""))

(defun org-roam--count-overlay-make (pos count)
  (let* ((overlay-value (propertize
                         (org-roam--number-to-superscript count)
                         'face 'org-roam-count-overlay-face))
         (ov (make-overlay pos pos (current-buffer) nil t)))
    (overlay-put ov 'roam-backlinks-count count)
    (overlay-put ov 'priority 1)
    (overlay-put ov 'after-string overlay-value)))

(defun org-roam--count-overlay-remove-all ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'roam-backlinks-count)
      (delete-overlay ov))))

(defun org-roam--count-overlay-query (id)
  "Return backlink count for node with ID."
  (caar (emacsql (vulpea-db)
                 [:select (funcall count source)
                  :from links
                  :where (and (= dest $s1)
                              (= type "id"))]
                 id)))

(defun org-roam--count-overlay-make-all ()
  (save-excursion
    (goto-char (point-min))
    (org-roam--count-overlay-remove-all)
    ;; File-level node: :PROPERTIES: drawer at top with #+title: below
    (when (looking-at "^:properties:\n\\(?:.*\n\\)*?:id:\\s-+\\([^[:space:]\n]+\\)")
      (when-let* ((id (string-trim (substring-no-properties (match-string 1))))
                  (count (org-roam--count-overlay-query id)))
        (when (and (< 0 count)
                   (re-search-forward "^#\\+title:\\s-+" nil t))
          (org-roam--count-overlay-make (point) count)
          (goto-char (point-min)))))
    ;; Heading-level nodes
    (while (re-search-forward "^\\(*+ \\)\\(.*$\\)\n\\(:properties:\\)\n\\(?:.*\n\\)*?:id:\\s-+\\([^[:space:]\n]+\\)" nil :no-error)
      (when-let* ((pos (match-beginning 2))
                  (id (string-trim (substring-no-properties (match-string 4))))
                  (count (org-roam--count-overlay-query id)))
        (when (< 0 count)
          (org-roam--count-overlay-make pos count))))))

;;;###autoload
(define-minor-mode org-roam-count-overlay-mode
  "Display backlink count for org-roam links."
  :after-hook
  (if org-roam-count-overlay-mode
      (progn
        (org-roam--count-overlay-make-all)
        (add-hook 'after-save-hook #'org-roam--count-overlay-make-all nil t))
    (org-roam--count-overlay-remove-all)
    (remove-hook 'after-save-hook #'org-roam--count-overlay-remove-all t)))


;;;###autoload
(defun vulpea-forward-links (&optional other-window)
  "Select and visit a note linked from the current note."
  (interactive "P")
  (let* ((id (or (when-let* ((link (org-element-context))
                             ((eq (org-element-type link) 'link))
                             ((string= (org-element-property :type link) "id")))
                   (org-element-property :path link))
                 (org-entry-get nil "ID" t)
                 (user-error "No note at point")))
         (links (seq-filter
                 (lambda (l) (string= (plist-get l :type) "id"))
                 (vulpea-db-query-links-from id)))
         (dest-ids (seq-uniq (mapcar (lambda (l) (plist-get l :dest)) links)))
         (dest-notes (delq nil (mapcar #'vulpea-db-get-by-id dest-ids)))
         (chosen (if dest-notes
                     (let ((consult-preview-key 'any))
                       (vulpea-select-from "Forward link: " dest-notes
                                           :require-match t))
                   (user-error "No forward links found"))))
    (when (and chosen (vulpea-note-id chosen))
      (vulpea-visit chosen other-window))))

;;;###autoload
(defun vulpea-backlinks (&optional other-window)
  "Select from list of all notes that link to a chosen note.
When visiting a source, creates a sparse tree showing all backlink locations.

Intelligently detects the note at point: if point is on an org ID link,
uses the link target; otherwise uses the current entry's ID (inherited).
The detected note pre-fills the first selection prompt.

After selecting the target note and a source note, visits the source
and calls `org-occur' to reveal all headings containing links to the
target — showing every backlink location, not just the first."
  (interactive "P")
  (let* (;; Detect note at point from link context or current entry ID
         (note-at-point
          (ignore-errors
            (if-let* ((link (org-element-context))
                      ((eq (org-element-type link) 'link))
                      ((string= (org-element-property :type link) "id"))
                      (id (org-element-property :path link)))
                (vulpea-db-get-by-id id)
              (when-let ((id (org-entry-get nil "ID" t)))
                (vulpea-db-get-by-id id)))))
         ;; Pre-compute note IDs that have backlinks (hash table for O(1) lookup)
         (ids-with-backlinks
          (let ((ht (make-hash-table :test 'equal)))
            (dolist (row (emacsql (vulpea-db)
                                  [:select :distinct [dest]
                                   :from links
                                   :where (= type "id")]))
              (puthash (car row) t ht))
            ht))
         ;; First selection: pick target note (filtered to those with backlinks)
         (target-note
          (vulpea-select "Backlinks for: "
                         :require-match t
                         :initial-prompt
                         (ignore-errors (vulpea-note-title note-at-point))
                         :filter-fn
                         (lambda (note)
                           (gethash (vulpea-note-id note) ids-with-backlinks))))
         (target-id (vulpea-note-id target-note)))
    (unless target-id
      (user-error "No valid note selected"))
    ;; Stash target-id for embark-collect actions (the second prompt's
    ;; candidates end up in the collect buffer, but the sparse tree
    ;; logic needs the target to build the [[id:...]] regexp).
    (setq vulpea-backlinks--target-id target-id)
    ;; Get backlinks and resolve source notes
    (let* ((backlinks (seq-filter
                       (lambda (l) (string= (plist-get l :type) "id"))
                       (vulpea-db-query-links-to target-id)))
           (source-ids (seq-uniq
                        (mapcar (lambda (l) (plist-get l :source)) backlinks)))
           (source-notes (delq nil (mapcar #'vulpea-db-get-by-id source-ids)))
           ;; Second selection: pick source to visit (with instant preview)
           (chosen-note
            (if source-notes
                (let ((consult-preview-key 'any))
                  (vulpea-select-from
                   (format "Backlink to '%s': " (vulpea-note-title target-note))
                   source-notes
                   :require-match t))
              (user-error "No backlinks found"))))
      (when (and chosen-note (vulpea-note-id chosen-note))
        (vulpea-visit chosen-note other-window)
        (vulpea-backlinks-sparse-tree target-id)))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org drawer lint     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-drawer-lint--in-block-p ()
  "Return non-nil if point is inside an Org block."
  (save-excursion
    (let ((case-fold-search t))
      (and (re-search-backward "^[[:blank:]]*#\\+\\(begin\\|end\\)_" nil t)
           (looking-at "^[[:blank:]]*#\\+begin_")))))

(defun org-drawer-lint--regex-ids ()
  "Return alist of (LINE . ID) for ID-like properties found by regex.
Detects both normal (own-line) and collapsed (single-line drawer) IDs.
Skips matches inside Org blocks."
  (let (results)
    (save-excursion
      ;; Normal: :ID: on its own line
      (goto-char (point-min))
      (while (re-search-forward
              "^[[:blank:]]*:ID:[[:blank:]]+\\(.+\\)" nil t)
        (let ((line (line-number-at-pos))
              (id (string-trim (match-string-no-properties 1))))
          (unless (org-drawer-lint--in-block-p)
            (push (cons line id) results))))
      ;; Collapsed: :PROPERTIES: ... :ID: ... :END: on one line
      (goto-char (point-min))
      (while (re-search-forward
              "^[[:blank:]]*:PROPERTIES:.*:ID:[[:blank:]]+\\([^[:blank:]\n]+\\).*:END:"
              nil t)
        (let ((line (line-number-at-pos))
              (id (match-string-no-properties 1)))
          (unless (or (org-drawer-lint--in-block-p)
                      (rassoc id results))
            (push (cons line id) results)))))
    (nreverse results)))

(defun org-drawer-lint--parsed-ids ()
  "Return list of IDs recognized by `org-element' parser."
  (let ((ast (org-element-parse-buffer))
        results)
    (org-element-map ast 'node-property
      (lambda (prop)
        (when (string= "ID" (upcase (org-element-property :key prop)))
          (push (string-trim (org-element-property :value prop)) results))))
    (nreverse results)))

;;;###autoload
(defun org-drawer-lint-check ()
  "Check current buffer for malformed property drawers.
Compares IDs found by regex against those recognized by
`org-element'.  Returns list of (LINE . ID) for broken ones."
  (interactive)
  (let* ((regex-ids (org-drawer-lint--regex-ids))
         (parsed-ids (org-drawer-lint--parsed-ids))
         (orphans (seq-filter
                   (lambda (pair)
                     (not (member (cdr pair) parsed-ids)))
                   regex-ids)))
    (when (called-interactively-p 'any)
      (if orphans
          (message "Drawer lint: %d broken ID(s) at line(s) %s"
                   (length orphans)
                   (mapconcat (lambda (p) (number-to-string (car p)))
                              orphans ", "))
        (message "Drawer lint: all %d ID(s) OK" (length regex-ids))))
    orphans))

;;;###autoload
(defun org-drawer-lint-fix ()
  "Fix malformed property drawers in current buffer.
Handles collapsed (single-line) drawers, blank-line gaps around
and inside drawers, and indented drawer lines.
Returns the number of fixes applied."
  (interactive)
  (let ((fixed 0))
    (save-excursion
      ;; 1. Expand collapsed single-line drawers
      (goto-char (point-min))
      (while (re-search-forward
              "^[[:blank:]]*:PROPERTIES:[[:blank:]]+\\(.+\\)[[:blank:]]*:END:[[:blank:]]*$"
              nil t)
        (let ((content (match-string 1)))
          (replace-match
           (concat ":PROPERTIES:\n"
                   (string-trim
                    (replace-regexp-in-string
                     "[[:space:]]+\\(:[A-Za-z_][A-Za-z0-9_-]*:\\)"
                     "\n\\1"
                     (string-trim content)))
                   "\n:END:"))
          (cl-incf fixed)))

      ;; 2. Remove blank lines between heading and property drawer
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(\\*+ .+\\)\n\\(\n+\\)\\([[:blank:]]*:PROPERTIES:[[:blank:]]*$\\)"
              nil t)
        (replace-match "\\1\n\\3")
        (cl-incf fixed))

      ;; 3. Remove blank lines at file start before property drawer
      (goto-char (point-min))
      (when (looking-at "\n+\\(:PROPERTIES:[[:blank:]]*$\\)")
        (replace-match "\\1")
        (cl-incf fixed))

      ;; 4. Remove blank lines inside property drawers
      (goto-char (point-min))
      (while (re-search-forward "^[[:blank:]]*:PROPERTIES:[[:blank:]]*$" nil t)
        (let ((start (point))
              (end-marker (when (re-search-forward
                                "^[[:blank:]]*:END:[[:blank:]]*$" nil t)
                            (copy-marker (match-beginning 0)))))
          (when end-marker
            (goto-char start)
            (while (re-search-forward "\n[[:space:]]*\n" end-marker t)
              (replace-match "\n")
              (cl-incf fixed))
            (set-marker end-marker nil))))

      ;; 5. Remove indentation from property drawer lines
      (goto-char (point-min))
      (while (re-search-forward "^[[:blank:]]+:PROPERTIES:[[:blank:]]*$" nil t)
        (replace-match ":PROPERTIES:")
        (cl-incf fixed)
        (while (and (zerop (forward-line 1))
                    (not (eobp))
                    (not (looking-at "^[[:blank:]]*:END:")))
          (when (looking-at "^[[:blank:]]+\\(:.+\\)$")
            (replace-match "\\1")
            (cl-incf fixed)))
        (when (looking-at "^[[:blank:]]+:END:[[:blank:]]*$")
          (replace-match ":END:")
          (cl-incf fixed))))

    (when (called-interactively-p 'any)
      (message "Drawer lint: %d fix(es) applied" fixed))
    fixed))

;;;###autoload
(defun org-drawer-lint-before-save-h ()
  "Auto-fix malformed property drawers before saving.
Only acts in `org-mode' buffers within `org-roam-directory'."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (bound-and-true-p org-roam-directory)
             (string-prefix-p
              (expand-file-name org-roam-directory)
              (expand-file-name (buffer-file-name))))
    (let ((n (org-drawer-lint-fix)))
      (when (< 0 n)
        (message "Drawer lint: auto-fixed %d issue(s)" n)))))

;;;###autoload
(defun org-drawer-lint-scan (&optional dir)
  "Scan and fix all .org files in DIR for malformed property drawers.
DIR defaults to `org-roam-directory'.  Works in both interactive
and batch (`emacs --batch') contexts."
  (interactive)
  (let* ((dir (expand-file-name (or dir org-roam-directory)))
         (files (directory-files-recursively dir "\\.org$"))
         (total-fixed 0)
         (total-remaining 0)
         (files-fixed 0))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (let ((n (org-drawer-lint-fix)))
          (when (< 0 n)
            (cl-incf files-fixed)
            (cl-incf total-fixed n)
            (write-region (point-min) (point-max) file nil 'silent)
            (message "Fixed %d issue(s): %s" n (file-relative-name file dir)))
          (let ((remaining (org-drawer-lint-check)))
            (when remaining
              (cl-incf total-remaining (length remaining))
              (dolist (r remaining)
                (message "UNFIXED L%d %s: %s"
                         (car r) (cdr r) (file-relative-name file dir))))))))
    (message "Scanned %d file(s), fixed %d issue(s) in %d file(s), %d remaining"
             (length files) total-fixed files-fixed total-remaining)))

;;;###autoload
(defun org-attach-lint-check (&optional dir)
  "Check that attachments referenced by Org headings exist on disk.
Scans .org files in DIR (defaults to `org-roam-directory') for
headings with the :ATTACH: tag, then verifies the attachment
directory exists and listed files in ORG_ATTACH_FILES are present."
  (interactive)
  (require 'org-attach)
  (let* ((dir (expand-file-name (or dir org-roam-directory)))
         (attach-root (expand-file-name org-attach-id-dir dir))
         (files (directory-files-recursively dir "\\.org$"))
         (issues nil))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (let ((ast (org-element-parse-buffer)))
          (org-element-map ast 'headline
            (lambda (hl)
              (when (member org-attach-auto-tag (org-element-property :tags hl))
                (let* ((id (org-element-property :ID hl))
                       (line (line-number-at-pos (org-element-property :begin hl)))
                       (file-list
                        (or (org-element-property :ORG_ATTACH_FILES hl)
                            (let ((names (org-element-map hl 'link
                                           (lambda (l)
                                             (when (string= "attachment"
                                                            (org-element-property :type l))
                                               (org-element-property :path l)))
                                           nil nil 'headline)))
                              (when names
                                (string-join names ", "))))))
                  (if (null id)
                      (push (list file line "no :ID:" file-list) issues)
                    (let* ((attach-dir
                            (expand-file-name
                             (org-attach-id-uuid-folder-format id)
                             attach-root))
                           (info (or file-list "")))
                      (if (not (file-directory-p attach-dir))
                          (push (list file line "missing dir" info) issues)
                        (when file-list
                          (dolist (f (split-string file-list "," t "[[:space:]]+"))
(let ((full (expand-file-name f attach-dir)))
                              (unless (file-exists-p full)
                                (push (list file line
                                            (if (file-symlink-p full)
                                                "broken link"
                                              "missing file")
                                            f)
                                      issues)))))))))))))))
    (setq issues (nreverse issues))
    (when issues
      (let* ((locations (mapcar (lambda (e) (format "%s:%d" (car e) (cadr e)))
                                issues))
             (col (1+ (apply #'max (mapcar #'length locations))))
             (fmt (format "%%-%ds  %%-13s %%s" col)))
        (cl-loop for e in issues
                 for loc in locations
                 do (message fmt loc (caddr e) (or (cadddr e) "")))))
    (message "Attach lint: %d issue(s)" (length issues))
    issues))


;;;###autoload
(defun org-attach-delete-at-point ()
  "Remove all attachment data from the heading at point.
Deletes the attachment directory (if present), the :ATTACH: tag,
and the ORG_ATTACH_FILES property."
  (interactive)
  (require 'org-attach)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let* ((id (org-entry-get nil "ID"))
         (attach-dir (when id (org-attach-dir)))
         (files (org-entry-get nil "ORG_ATTACH_FILES")))
    (when (and attach-dir (file-directory-p attach-dir))
      (delete-directory attach-dir t)
      (message "Deleted dir: %s" attach-dir)
      ;; Remove parent dir if empty (the XX/ prefix dir)
      (let ((parent (file-name-directory (directory-file-name attach-dir))))
        (when (and (file-directory-p parent)
                   (null (directory-files parent nil "\\`[^.]" t)))
          (delete-directory parent))))
    (org-toggle-tag org-attach-auto-tag 'off)
    (org-entry-delete nil "ORG_ATTACH_FILES")
    (message "Attachment removed%s"
             (if files (format ": %s" files) ""))))

;;;###autoload
(defun vulpea-db-verify (&optional dir)
  "Verify that all filesystem IDs are indexed in the Vulpea DB.
Compares IDs recognized by `org-element' on disk against entries
in the DB.  DIR defaults to `org-roam-directory'.
Run after `vulpea-db-sync-full-scan' for meaningful results."
  (interactive)
  (let* ((dir (expand-file-name (or dir org-roam-directory)))
         (files (directory-files-recursively dir "\\.org$"))
         (fs-ids (make-hash-table :test 'equal))
         (db-ids (make-hash-table :test 'equal))
         missing stale)
    ;; Filesystem: IDs that org-element can parse (skip VULPEA_IGNORE)
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (let ((ast (org-element-parse-buffer)))
          (org-element-map ast 'node-property
            (lambda (prop)
              (when (string= "ID" (upcase (org-element-property :key prop)))
                (let* ((drawer (org-element-property :parent prop))
                       (ignored (org-element-map drawer 'node-property
                                  (lambda (p)
                                    (and (string= "VULPEA_IGNORE"
                                                  (upcase (org-element-property :key p)))
                                         (org-element-property :value p)))
                                  nil t)))
                  (unless ignored
                    (puthash (string-trim (org-element-property :value prop))
                             (file-relative-name file dir)
                             fs-ids)))))))))
    ;; DB: all indexed note IDs
    (dolist (row (emacsql (vulpea-db) [:select [id] :from notes]))
      (puthash (car row) t db-ids))
    ;; Diff
    (maphash (lambda (id file)
               (unless (gethash id db-ids)
                 (push (cons id file) missing)))
             fs-ids)
    (maphash (lambda (id _)
               (unless (gethash id fs-ids)
                 (push id stale)))
             db-ids)
    (when missing
      (message "Missing from DB (%d):" (length missing))
      (dolist (e missing)
        (message "  %s: %s" (cdr e) (car e))))
    (when stale
      (message "Stale in DB (%d):" (length stale))
      (dolist (id stale)
        (message "  %s" id)))
    (unless (or missing stale)
      (message "Vulpea DB OK: %d note(s) match filesystem"
               (hash-table-count db-ids)))))
