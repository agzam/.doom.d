;;; custom/org/autoload/org-roam-helpers.el -*- lexical-binding: t; -*-

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
        (let ((pos (point)))
          ;; Sparse tree: reveal all headings containing links to target
          (org-occur (format "\\[\\[id:%s\\]" (regexp-quote target-id)))
          ;; Restore position and scroll — org-occur folds everything
          ;; via org-cycle-overview, which leaves the window at the top
          (goto-char pos)
          (org-reveal)
          (recenter))))))
