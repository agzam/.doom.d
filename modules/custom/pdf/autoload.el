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
  (when (eq major-mode 'org-mode)
    (org-noter--with-valid-session
     (when-let ((_ (eq major-mode 'org-mode))
                (page-num (org-noter--doc-approx-location)))
       (org-set-property
        org-noter-property-note-location
        (number-to-string (car page-num)))
       (org-fold-hide-drawer-toggle 'off :no-error)))))


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
(defun org-noter-pdf-scroll-down ()
  "Scroll current pdf down.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (if (eq major-mode 'pdf-view-mode)
      (pdf-view-scroll-up-or-next-page)
    (org-noter--with-valid-session
     (when-let ((cur (current-buffer))
                (buf (org-noter--session-doc-buffer session)))
       (switch-to-buffer-other-window buf)
       (pdf-view-next-line-or-next-page 1)
       (switch-to-buffer-other-window cur)))))

;;;###autoload
(defun org-noter-pdf-scroll-up ()
  "Scroll current pdf up.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (if (eq major-mode 'pdf-view-mode)
      (pdf-view-scroll-down-or-previous-page)
    (org-noter--with-valid-session
     (when-let ((cur (current-buffer))
                (buf (org-noter--session-doc-buffer session)))
       (switch-to-buffer-other-window buf)
       (pdf-view-previous-line-or-previous-page 1)
       (switch-to-buffer-other-window cur)))))

;;;###autoload
(defun org-noter-pdf-next-page ()
  "Scroll current pdf to the next page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (if (eq major-mode 'pdf-view-mode)
      (pdf-view-next-page)
    (org-noter--with-valid-session
     (when-let ((cur (current-buffer))
                (buf (org-noter--session-doc-buffer session)))
       (switch-to-buffer-other-window buf)
       (pdf-view-next-page)
       (switch-to-buffer-other-window cur)))))

;;;###autoload
(defun org-noter-pdf-prev-page ()
  "Scroll current pdf to the previous page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (if (eq major-mode 'pdf-view-mode)
      (pdf-view-previous-page)
    (org-noter--with-valid-session
     (when-let ((cur (current-buffer))
                (buf (org-noter--session-doc-buffer session)))
       (switch-to-buffer-other-window buf)
       (pdf-view-previous-page)
       (switch-to-buffer-other-window cur)))))

;;;###autoload
(defun org-noter-top-of-the-page ()
  "Scroll current document to the top of current page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (if (eq major-mode 'pdf-view-mode)
      (pdf-evil-goto-first-line)
    (org-noter--with-valid-session
     (when-let ((cur (current-buffer))
                (buf (org-noter--session-doc-buffer session)))
       (switch-to-buffer-other-window buf)
       (let ((pdf-view-continuous nil))
         (image-scroll-down))
       (switch-to-buffer-other-window cur)))))

;;;###autoload
(defun org-noter-bottom-of-the-page ()
  "Scroll current document to the bottom of current page.
It's convenient to issue scrolling the document without having to
switch to it."
  (interactive)
  (if (eq major-mode 'pdf-view-mode)
      (pdf-evil-goto-last-line)
    (org-noter--with-valid-session
     (when-let ((cur (current-buffer))
                (buf (org-noter--session-doc-buffer session)))
       (switch-to-buffer-other-window buf)
       (let ((pdf-view-continuous nil))
         (image-scroll-up))
       (switch-to-buffer-other-window cur)))))

;;;###autoload
(defun org-noter-sync ()
  (interactive)
  (pcase major-mode
    ('pdf-view-mode (org-noter-sync-current-page-or-chapter))
    ('org-mode (org-noter-sync-current-note))))

;;;###autoload
(transient-define-prefix org-noter-transient ()
  "Org Noter"
  [[("N" "Noter start" org-noter)
    ("I" "new note" org-noter-insert-note)]
   [("n" "sync" org-noter-sync :transient t)
    ("a" "anchor" org-noter-anchor-to-current-page+)]
   [("j" "scroll up" org-noter-pdf-scroll-down :transient t)
    ("k" "scroll down" org-noter-pdf-scroll-up :transient t)]
   [("K" "prev page" org-noter-pdf-prev-page :transient t)
    ("J" "next page" org-noter-pdf-next-page :transient t)]
   [("gg" "page top" org-noter-top-of-the-page :transient t)
    ("G" "page bottom" org-noter-bottom-of-the-page :transient t)]]
  [:hide
   always
   [("i" "insert mode"
     (lambda ()
       (interactive)
       (transient-quit-all)
       (evil-insert-state)))]])
