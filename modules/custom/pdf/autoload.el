;;; custom/pdf/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pdf-view-current-progress ()
  "Show current progress in a pdf document."
  (interactive)
  (message "at %.1f%%"
           (* 100 (/ (float (pdf-view-current-page))
                     (float (pdf-info-number-of-pages))))))


;;;###autoload
(defun org-noter-anchor-to-current-page+ ()
  "Set NOTER_PAGE prop. of active heading to current document page number."
  (interactive)
  (org-noter--with-valid-session
   (when-let ((_ (eq major-mode 'org-mode))
              (page-num (org-noter--doc-approx-location)))
     (org-set-property
      org-noter-property-note-location
      (number-to-string (car page-num)))
     (org-fold-hide-drawer-toggle 'off :no-error))))

