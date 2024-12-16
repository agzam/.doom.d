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
  '((t :inherit org-list-dt :height 0.6 :underline nil :weight light))
  "Face for Org Roam count overlay.")

(defun org-roam--count-overlay-make (pos count)
  (let* ((overlay-value (propertize
                         (format "│%d│ " count)
                         'face 'org-roam-count-overlay-face
                         'display '(raise 0.3)))
         (ov (make-overlay pos pos (current-buffer) nil t)))
    (overlay-put ov 'roam-backlinks-count count)
    (overlay-put ov 'priority 1)
    (overlay-put ov 'after-string overlay-value)))

(defun org-roam--count-overlay-remove-all ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'roam-backlinks-count)
      (delete-overlay ov))))

(defun org-roam--count-overlay-make-all ()
  (save-excursion
    (goto-char (point-min))
    (org-roam--count-overlay-remove-all)
    (while (re-search-forward "^\\(*+ \\)\\(.*$\\)\n\\(:properties:\\)\n\\(?:.*\n\\)*?:id:\\s-+\\([^[:space:]\n]+\\)" nil :no-error)
      (when-let* ((pos (match-beginning 2))
                  (id (string-trim (substring-no-properties (match-string 4))))
                  (count (caar
                          (org-roam-db-query
                           [:select (funcall count source)
                            :from links
                            :where (= dest $s1)
                            :and (= type "id")]
                           id))))
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
(defun consult-org-roam-backlinks* (&optional other-window)
  "Select from list of all notes that link to
the current note with precise positions."
  ;; this is an attempt to fix: jgru/consult-org-roam#38
  (interactive current-prefix-arg)
  (let* ((node (org-roam-node-at-point))
         (backlinks (org-roam-db-query
                     [:select [source pos]
                      :from links
                      :where (= dest $s1)
                      :and (= type "id")]
                     (if node
                         (org-roam-node-id (org-roam-node-at-point))
                       (user-error "Buffer does not contain org-roam-nodes"))))
         (source-ids (mapcar #'car backlinks))
         (pos-map (make-hash-table :test 'equal))
         (_ (dolist (link backlinks)
              (puthash (car link) (cadr link) pos-map)))
         (chosen-node-or-str (if source-ids
                                 (consult-org-roam-node-read ""
                                                             (lambda (n)
                                                               (if (org-roam-node-p n)
                                                                   (if (member (org-roam-node-id n) source-ids)
                                                                       t
                                                                     nil))))
                               (user-error "No backlinks found"))))
    (when chosen-node-or-str
      (org-roam-node-visit chosen-node-or-str other-window)
      (goto-char (gethash (org-roam-node-id chosen-node-or-str) pos-map))
      (org-reveal))))
