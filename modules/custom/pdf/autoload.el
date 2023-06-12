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


;;;###autoload
(defun pdf-toggle-continuous-scroll ()
  "Toggle between single page and scrollable document."
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (setq pdf-view-continuous (not pdf-view-continuous))))


;;;###autoload
(defun pdf-evil-goto-first-line ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (funcall
     (if pdf-view-continuous
         #'pdf-view-first-page
       #'image-scroll-down))))

;;;###autoload
(defun pdf-evil-goto-last-line ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (funcall
     (if pdf-view-continuous
         #'pdf-view-last-page
       #'image-scroll-up))))


;;;###autoload
(defun org-noter-pdf-next-page ()
  "Scroll current pdf to the next page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (org-noter--with-valid-session
   (when-let ((cur (current-buffer))
              (buf (org-noter--session-doc-buffer session)))
     (switch-to-buffer-other-window buf)
     (pdf-view-next-page)
     (switch-to-buffer-other-window cur))))

;;;###autoload
(defun org-noter-pdf-prev-page ()
  "Scroll current pdf to the previous page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (org-noter--with-valid-session
   (when-let ((cur (current-buffer))
              (buf (org-noter--session-doc-buffer session)))
     (switch-to-buffer-other-window buf)
     (pdf-view-previous-page)
     (switch-to-buffer-other-window cur))))

;;;###autoload
(defun org-noter-top-of-the-page ()
  "Scroll current document to the top of current page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (org-noter--with-valid-session
   (when-let ((cur (current-buffer))
              (buf (org-noter--session-doc-buffer session)))
     (switch-to-buffer-other-window buf)
     (let ((pdf-view-continuous nil))
       (image-scroll-down))
     (switch-to-buffer-other-window cur))))

;;;###autoload
(defun org-noter-bottom-of-the-page ()
  "Scroll current document to the bottom of current page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (org-noter--with-valid-session
   (when-let ((cur (current-buffer))
              (buf (org-noter--session-doc-buffer session)))
     (switch-to-buffer-other-window buf)
     (let ((pdf-view-continuous nil))
       (image-scroll-up))
     (switch-to-buffer-other-window cur))))
