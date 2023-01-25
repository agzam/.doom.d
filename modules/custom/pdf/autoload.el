;;; custom/pdf/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun pdf-view-current-progress ()
  "Show current progress in a pdf document."
  (interactive)
  (message "at %.1f%%"
           (* 100 (/ (float (pdf-view-current-page))
                     (float (pdf-info-number-of-pages))))))
