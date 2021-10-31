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

;;;###autoload (autoload '+hydra/text-zoom/body "custom/spacemacsy/autoload/windows" nil t)
(defhydra +hydra/text-zoom (:hint nil :color red)
  "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
  ("j" doom/increase-font-size "in")
  ("k" doom/decrease-font-size "out")
  ("0" doom/reset-font-size "reset")
  ("h" toggle-frame-full-height "stretch vertically" :exit t)
  ("c" center-frame-horizontally "center frame horizontally" :exit t)
  ("m" toggle-frame-maximized-undecorated "maximize frame" :exit t))
