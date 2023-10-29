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
