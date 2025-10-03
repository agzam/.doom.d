;;; custom/pdf/pdf.el -*- lexical-binding: t; -*-

;;;###autoload
(defun adjust-pdf-colors-on-theme-change-h (_)
  "Keep pdf-view-themed buffers in sync with main color-theme changes."
  (thread-last
    (buffer-list)
    (seq-filter
     (lambda (b)
       (with-current-buffer b (eq major-mode 'pdf-view-mode))))
    (seq-do
     (lambda (b)
       (with-current-buffer b
         (when pdf-view-themed-minor-mode
           (pdf-view-themed-minor-mode +1)))))))
