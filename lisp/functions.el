;;; lisp/functions.el -*- lexical-binding: t; -*-

(defun display-buffer-in-quadrant (buffer alist)
  "Display BUFFER in a side window while preserving existing window dimensions.
When displaying BUFFER for the first time, creates a new window using a quarter
- 25% of the frame width. If BUFFER is already displayed, reuses the existing
window without modifying its dimensions.

The side is determined by the direction entry, e.g., (direction . right)
Initial width controlled by init-width entry, e.g., (init-width . 0.10)
- would occupy the 10% of the frame width

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'. It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((existing (get-buffer-window buffer))
        (init-w (alist-get 'init-width alist 0.25)))
    (if existing
        (display-buffer-reuse-window buffer alist)
      (when-let ((window (display-buffer-in-direction buffer alist)))
        (with-selected-window window
          (window-resize window
                         (- (round (* init-w (frame-pixel-width)))
                            (window-width window t))
                         t nil t))
        window))))
