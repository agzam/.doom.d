;;; custom/general/autoload/frames.el -*- lexical-binding: t; -*-

;;;###autoload
(defun display-current-workarea ()
  "Returns x,y width, height of active monitor of current-frame."
  (let* ((current-frame (selected-frame))
         (monitor-at-pos (car (seq-filter
                               (lambda (monitor)
                                 (let ((geometry (cdr (assoc 'geometry monitor))))
                                   (and (<= (car geometry) (frame-parameter current-frame 'left))
                                        (<= (+ (car geometry) (cadr geometry)) (frame-parameter current-frame 'left)))))
                               (display-monitor-attributes-list)))))
    (alist-get 'workarea monitor-at-pos)))

;;;###autoload
(defun display-workarea-height ()
  "Returns current monitor height."
  (nth 3 (display-current-workarea)))

;;;###autoload
(defun display-workarea-width ()
  "Returns current monitor width."
  (nth 2 (display-current-workarea)))

;;;###autoload
(defun toggle-frame-maximized-undecorated ()
  (interactive)
  (posframe-delete-all)
  (+corfu-kill-frames)
  (let* ((frame (selected-frame))
         (on? (and (frame-parameter frame 'undecorated)
                   (eq (frame-parameter frame 'fullscreen) 'maximized)))
         (x (nth 0 (display-current-workarea)))
         (y (nth 1 (display-current-workarea)))
         (ns-menu-autohide (bound-and-true-p ns-auto-hide-menu-bar))
         (cut (if on?
                  (if ns-menu-autohide 26 25)
                (if ns-menu-autohide 4 26))))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- (display-workarea-height) cut) nil t)
    (set-frame-width frame (- (display-workarea-width) 10) nil t)
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
                           (number-to-string (or percentage 80)))))
         (x-pos (round (* (display-workarea-width) (* (/ (- 100 stretch-ratio) 2) 0.01))))
         (width (round (* (display-workarea-width) (* stretch-ratio 0.01)))))
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
  (+corfu-kill-frames)
  (let* ((fr (selected-frame)))
    (if (frame-parameter fr 'undecorated-fullheight)
        (progn
          (set-frame-parameter fr 'undecorated-fullheight nil)
          (set-frame-parameter fr 'undecorated nil)
          (set-frame-parameter fr 'fullscreen nil))
      (reset-ns-autohide-menu-bar)
      (progn
        (set-frame-parameter fr 'undecorated t)
        (set-frame-parameter fr 'undecorated-fullheight t)
        (set-frame-position fr (car (frame-position)) 26)
        (set-frame-height fr (- (display-workarea-height) (tab-bar-height nil t)) nil :pixelwise)))
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
  (set-frame-width nil (- (frame-width) (or delta 3))))

;;;###autoload
(defun reduce-frame-height (&optional delta)
  (interactive)
  (when transient--window
    (quit-restore-window transient--window))
  (set-frame-height nil (- (frame-height) (or delta 1))))

;;;###autoload
(defun screen-width-height ()
  "Returns width and height of current monitor in pixels."
  (pcase-let ((`(_ _ _ ,h ,v)
               (assq 'geometry (frame-monitor-attributes))))
    (list h v)))

;;;###autoload
(defun widen-frame-width (&optional delta)
  (interactive)
  (when transient--window
    (quit-restore-window transient--window))
  (let ((disp-width (car (screen-width-height))))
    (if (< (frame-native-width) disp-width)
        (set-frame-width nil (+ (frame-width) (or delta 3)))
      (set-frame-width nil disp-width nil :pixelwise))))

;;;###autoload
(defun increase-frame-height (&optional delta)
  (interactive)
  (when transient--window
    (quit-restore-window transient--window))
  (let ((disp-height (- (cadr (screen-width-height)) 30)))
    (if (< (frame-native-height) disp-height)
        (set-frame-height nil (+ (frame-height) (or delta 1)))
      (set-frame-height nil disp-height nil :pixelwise))))

;;;###autoload
(defun place-frame-at-display-spot (specs &optional frame)
  "Position and resize the FRAME according SPECS.
SPECS is a list of x, y, width & height.
See: `(frame-position-display-spots)'  details."
  (interactive)
  (pcase-let* ((pw (display-workarea-width))
               (ph (display-workarea-height))
               (`(,x ,y ,w ,h) (seq-mapn
                                (lambda (val kind)
                                  (if (eq 'float (type-of val))
                                      (cl-case kind
                                        ((or :x :w) (floor (* pw val)))
                                        (:y (floor (* ph val)))
                                        (:h (let* ((tabs-h (tab-bar-height frame t)))
                                              (if (eq val 1.0)
                                                  (- ph tabs-h)
                                                (floor (- (* ph val) tabs-h))))))
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
    ("f" "full-screen" toggle-frame-fullscreen+)]
   [("H" "shrink frame" shrink-frame-width :transient t)
    ("L" "widen frame" widen-frame-width :transient t)
    ("J" "reduce frame height" reduce-frame-height :transient t)
    ("K" "increase frame height" increase-frame-height :transient t)]]
  [:hide always
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'frame-zoom-transient
      (seq-map-indexed
       (lambda (geom idx)
         (list (number-to-string (1+ idx))
               (format "Layout %s" (1+ idx))
               (lambda ()
                 (interactive)
                 (place-frame-at-display-spot geom))
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
         (0 0 0.830 1.0)
         (0.166 0 0.664 1.0)
         (0.332 0 0.8 1.0)
         (0.5 0 0.5 1.0)
         (0.664 0 0.35 1.0)))))])
