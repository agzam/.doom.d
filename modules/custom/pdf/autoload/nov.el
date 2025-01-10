;;; custom/pdf/autoload/nov.el -*- lexical-binding: t; -*-

;;;###autoload
(defun nov-back-or-quit ()
  (interactive)
  (if nov-history
      (nov-history-back)
    (kill-current-buffer)))
