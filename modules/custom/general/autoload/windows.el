;;; custom/general/autoload/windows.el -*- lexical-binding: t; -*-

(require 'hydra)

(defun delete-other-windows-horizontally ()
  "Delete all windows to the left and right of the current
window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun toggle-window-divider ()
  (interactive)
  (setf right-divider-width (if window-divider-mode 1 6))
  (setf left-divider-width (if window-divider-mode 1 6))
  (window-divider-mode 'toggle))
