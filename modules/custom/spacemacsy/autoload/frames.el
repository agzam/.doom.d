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
                (if ns-auto-hide-menu-bar 4 26)))
         (header-h (+ (tab-bar-height nil t) cut)))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- display-height header-h) nil t)
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

(defun reset-ns-autohide-menu-bar ()
  "In OSX frame resizing could affects ns-menu. This makes sure it
remains shown or hidden - whatever the previous value was."
  (when (eq system-type 'darwin)
   (let ((val ns-auto-hide-menu-bar))
     (setf ns-auto-hide-menu-bar (not val))
     (setf ns-auto-hide-menu-bar val))))

;;;###autoload (autoload 'toggle-frame-full-height "custom/spacemacsy/autoload/frames" nil t)
(defun toggle-frame-full-height ()
  "Removes the title of the current frame and stretches it out to
  the display height. To be used on a Mac."
  (interactive)
  (let ((tbh (tab-bar-height nil t)))
    (if (frame-parameter nil 'undecorated-fullheight)
        (progn
          (set-frame-parameter nil 'undecorated-fullheight nil)
          (set-frame-parameter nil 'undecorated nil))
      (reset-ns-autohide-menu-bar)
      (progn
        (set-frame-parameter nil 'undecorated t)
        (set-frame-parameter nil 'undecorated-fullheight t)
        (set-frame-position nil (car (frame-position)) (+ 1 tbh))
        (set-frame-height nil (- (x-display-pixel-height) (+ tbh 29)) nil :pixelwise)))
    (redraw-display)))

(defun reset-frame-full-height ()
  (toggle-frame-full-height)
  (toggle-frame-full-height))

;;;###autoload (autoload '+hydra/text-zoom/body "custom/spacemacsy/autoload/frames" nil t)
(defhydra +hydra/text-zoom (:color amarath
                            :hint nil
                            :before-exit (reset-frame-full-height))
  "
^Zoom^             ^Resize^
---------------------------------
_j_: in            _h_: full-height
_k_: out           _c_: center
_0_: reset         _m_: maximize
^^                 _f_: fullscreen
"
  ("j" doom/increase-font-size)
  ("k" doom/decrease-font-size)
  ("0" doom/reset-font-size)
  ("h" toggle-frame-full-height :exit t)
  ("c" center-frame-horizontally :exit t)
  ("m" toggle-frame-maximized-undecorated :exit t)
  ("f" toggle-frame-fullscreen :exit t))
