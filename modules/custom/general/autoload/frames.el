;;; custom/general/autoload/frames.el -*- lexical-binding: t; -*-

;;;###autoload
(defun toggle-frame-maximized-undecorated ()
  (interactive)
  (posframe-delete-all)
  (let* ((frame (selected-frame))
         (on? (and (frame-parameter frame 'undecorated)
                   (eq (frame-parameter frame 'fullscreen) 'maximized)))
         (geom (frame-monitor-attribute 'geometry))
         (x (nth 0 geom))
         (y (nth 1 geom))
         (display-height (nth 3 geom))
         (display-width (nth 2 geom))
         (ns-menu-autohide (bound-and-true-p ns-auto-hide-menu-bar))
         (cut (if on?
                  (if ns-menu-autohide 26 50)
                (if ns-menu-autohide 4 26)))
         (header-h (+ (tab-bar-height nil t) cut)))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- display-height header-h) nil t)
    (set-frame-width frame (- display-width 20) nil t)
    (set-frame-position frame x y)))

;;;###autoload
(defun center-frame-horizontally (&optional prompt percentage)
  "Positions the current frame in the middle of the screen,
vertically stretching it from top to bottom. Useful on ultra-wide
monitor.  With universal argument prompts for the percentage -
the horizontal screen estate the frame should occupy."
  (interactive "P")
  (posframe-delete-all)
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
  "In OSX frame resizing could affects ns-menu. This makes sure
it remains shown or hidden - whatever the previous value was."
  (when (and (eq system-type 'darwin)
             (boundp 'ns-auto-hide-menu-bar))
    (let ((val ns-auto-hide-menu-bar))
      (setf ns-auto-hide-menu-bar (not val))
      (setf ns-auto-hide-menu-bar val))))

;;;###autoload
(defun toggle-frame-full-height ()
  "Removes the title of the current frame and stretches it out to
  the display height. To be used on a Mac."
  (interactive)
  (posframe-delete-all)
  (let* ((fr (selected-frame))
         (tbh (tab-bar-height fr t)))
    (if (frame-parameter fr 'undecorated-fullheight)
        (progn
          (set-frame-parameter fr 'undecorated-fullheight nil)
          (set-frame-parameter fr 'undecorated nil)
          (set-frame-parameter fr 'fullscreen nil))
      (reset-ns-autohide-menu-bar)
      (progn
        (set-frame-parameter fr 'undecorated t)
        (set-frame-parameter fr 'undecorated-fullheight t)
        (set-frame-position fr (car (frame-position)) (+ 1 tbh))
        (set-frame-height fr (- (x-display-pixel-height) (+ tbh 26)) nil :pixelwise)))
    (redraw-display)))

;;;###autoload
(defun reset-frame-full-height ()
  (posframe-delete-all)
  (unless (eq 'fullboth (frame-parameter nil 'fullscreen))
    (toggle-frame-full-height)
    (toggle-frame-full-height)))

;;;###autoload
(defun toggle-frame-fullscreen+ ()
  (interactive)
  (toggle-frame-fullscreen)
  (posframe-delete-all))

;;;###autoload
(defun shrink-frame-width (&optional delta)
  (interactive)
  (when transient--window
    (quit-restore-window transient--window))
  (set-frame-width nil (- (frame-width) (or delta 5))))

;;;###autoload
(defun widen-frame-width (&optional delta)
  (interactive)
  (when transient--window
    (quit-restore-window transient--window))
  (set-frame-width nil (+ (frame-width) (or delta 5))))


;;;###autoload
(defun place-frame-at-display-spot (specs &optional frame)
  "Position and resize the FRAME according SPECS.
SPECS is a list of x, y, width & height.
See: `(frame-position-display-spots)'  details."
  (interactive)
  (pcase-let* ((pw (display-pixel-width))
               (ph (display-pixel-height))
               (`(,x ,y ,w ,h) (seq-mapn
                                (lambda (val kind)
                                  (if (eq 'float (type-of val))
                                      (cl-case kind
                                        ((or :x :w) (floor (* pw val)))
                                        (:y (floor (* ph val)))
                                        (:h (let* ((tabs-h (tab-bar-height frame t)))
                                              (if (eq val 1.0)
                                                  (- ph (+ 26 tabs-h))
                                                (floor (- (* ph val) (+ 26 tabs-h)))))))
                                    val))
                                specs '(:x :y :w :h)))
               (x (cond ((< x 0) 0) ((< pw x) pw) (t x)))
               (y (cond ((< y 0) 0) ((< ph y) ph) (t y)))
               (w (cond ((< pw (+ x w)) (- pw x)) (t w)))
               (h (cond ((< ph (+ y h)) (- ph y)) (t h))))
    (when transient--window
      (quit-restore-window transient--window))
    (set-frame-position frame x y)
    (message (format "position: %s, %s; width: %s, height: %s" x y w h))
    (set-frame-size frame w h t)))

;;;###autoload
(require 'transient)

;;;###autoload
(transient-define-prefix frame-zoom-transient ()
  "Text Zoom"
  ["Frame Text Zoom"
   [("j" "decrease font" doom/decrease-font-size :transient t)
    ("k" "increase font" doom/increase-font-size :transient t)
    ("0" "reset font size" doom/reset-font-size :transient t)]
   [("s-j" "text scale down" text-scale-decrease :transient t)
    ("s-k" "text scale up" text-scale-increase :transient t)
    ("s-0" "text scale reset" text-scale-set :transient t)]
   [("h" "full height" toggle-frame-full-height)
    ("c" "center frame horizontally" center-frame-horizontally)
    ("m" "maximize frame" toggle-frame-maximized-undecorated)
    ("f" "full-screen" toggle-frame-fullscreen+)
    ("H" "shrink frame" shrink-frame-width :transient t)
    ("L" "widen frame" widen-frame-width :transient t)]]
  [:hide always ,@(seq-map-indexed
                   (lambda (elt idx)
                     `(,(number-to-string (+ 1 idx))
                       ,(format "layout %s" (+ 1 idx))
                       (lambda ()
                         (interactive)
                         (place-frame-at-display-spot (quote ,elt)))
                       :transient t))
                   ;; list of pre-sets for different positions of Emacs frame on the
                   ;; screen. Each pre-set is a list of X, Y, WIDTH, and HEIGHT.
                   ;;
                   ;; When a value is a decimal - it represents the discrete pixel
                   ;; position on the display. Also, the numbers can be floats, in that
                   ;; case - it represent the proportional value, e.g., width of 0.25
                   ;; means quarter of `(display-pixel-width)'.
                   '((0 0 0.332 1.0)
                     (0 0 0.498 1.0)
                     (0 0 0.664 1.0)
                     (0.166 0 0.664 1.0)
                     (0.332 0 0.8 1.0)
                     (0.5 0 0.5 1.0)
                     (0.664 0 0.35 1.0)))])
