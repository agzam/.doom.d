;;; custom/spacemacsy/autoload/windows.el -*- lexical-binding: t; -*-

(require 'hydra)

(defun toggle-frame-full-height ()
  "Removes the title of the current frame and stretches it out to
  the display height. To be used on a Mac."
  (interactive)
  (if (frame-parameter nil 'undecorated)
      (set-frame-parameter nil 'undecorated nil)
    (progn
      (set-frame-parameter nil 'undecorated t)
      (set-frame-position nil (car (frame-position)) 0)
      (set-frame-height nil (- (x-display-pixel-height) 29) nil :pixelwise)))
  (redraw-display))

(defun center-frame-horizontally (&optional prompt percentage)
  "Positions the current frame in the middle of the screen,
vertically stretching it from top to bottom. Useful on ultra-wide monitor.
With universal argument prompts for the percentage - the horizontal screen estate the frame should occupy."
  (interactive "P")
  (let* ((stretch-ratio (string-to-number
                         (if prompt
                             (completing-read "Choose: " '("50%" "70%" "80%" "90%") nil t)
                           (number-to-string (or percentage 70)))))
         (x-pos (round (* (x-display-pixel-width) (* (/ (- 100 stretch-ratio) 2) 0.01))))
         (width (round (* (x-display-pixel-width) (* stretch-ratio 0.01)))))
    (set-frame-position nil x-pos 0)
    (set-frame-width nil width nil t)
    (when (not (frame-parameter nil 'undecorated))
      (toggle-frame-full-height))
    (redraw-display)))

(defun alternate-buffer ()
  (interactive)
  (persp-add-buffer (current-buffer))
  (when-let ((b (evil-alternate-buffer (get-buffer-window))))
    (switch-to-buffer (car b))))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;;;###autoload (autoload '+hydra/text-zoom/body "custom/spacemacsy/autoload/windows" nil t)
(defhydra +hydra/text-zoom (:hint nil :color red)
  "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
  ("j" doom/increase-font-size "in")
  ("k" doom/decrease-font-size "out")
  ("0" doom/reset-font-size "reset")
  ("h" toggle-frame-full-height "stretch vertically" :exit t)
  ("c" center-frame-horizontally "center frame horizontally" :exit t))
