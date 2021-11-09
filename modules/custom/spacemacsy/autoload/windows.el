;;; custom/spacemacsy/autoload/windows.el -*- lexical-binding: t; -*-

(require 'hydra)

(defun delete-other-windows-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

