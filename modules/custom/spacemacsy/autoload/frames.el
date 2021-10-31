;;; custom/spacemacsy/autoload/frames.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'toggle-frame-maximized-undecorated "custom/spacemacsy/autoload/frames" nil t)
(defun toggle-frame-maximized-undecorated ()
  (interactive)
  (let* ((frame (selected-frame))
         (on? (and (frame-parameter frame 'undecorated)
                   (eq (frame-parameter frame 'fullscreen) 'maximized)))
         (geom (frame-monitor-attribute 'geometry))
         (x (nth 0 geom))
         (y (nth 1 geom))
         (display-height (nth 3 geom))
         (display-width (nth 2 geom))
         (cut (if on?
                  (if ns-auto-hide-menu-bar 26 50)
                (if ns-auto-hide-menu-bar 4 26))))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- display-height cut) nil t)
    (set-frame-width frame (- display-width 20) nil t)
    (set-frame-position frame x y)))

;;;###autoload (autoload 'center-frame-horizontally "custom/spacemacsy/autoload/frames" nil t)
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

;;;###autoload (autoload 'toggle-frame-full-height "custom/spacemacsy/autoload/frames" nil t)
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
