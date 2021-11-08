;;; custom/spacemacsy/autoload/narrow.el -*- lexical-binding: t; -*-

(defun clone-indirect-buffer-de-activate-mark ()
  "This is a workaround for the evil visual state error message like:
Error in post-command-hook (evil-visual-post-command):
(error \"Marker points into wrong buffer\" #<marker at 27875 in .spacemacs<2>>)"
  (let ((region-was-active (region-active-p)))
    (when region-was-active (deactivate-mark))
    (call-interactively 'clone-indirect-buffer)
    (when region-was-active (activate-mark))))

(defun narrow-to-indirect-buffer (narrower target-name)
  "Use the function `narrower' to narrow within an indirect buffer, except where
the starting buffer is in a state (such as visual block mode) that would cause
this to work incorrectly. `target-name' is the string name of the entity being
narrowed to."
  ;; There may be a way to get visual block mode working similar to the
  ;; workaround we did for visual line mode; this usecase however seems like an
  ;; edgecase at best, so let's patch it if we find out it's needed; otherwise
  ;; let's not hold up the base functionality anymore.
  (if (and (eq evil-state 'visual) (eq evil-visual-selection 'block))
      (message "Cannot narrow to indirect buffer from visual block mode.")
    (when evil-ex-active-highlights-alist
      (evil-ex-nohighlight))
    (clone-indirect-buffer-de-activate-mark)
    (call-interactively narrower)
    (message (format "%s narrowed to an indirect buffer" target-name))))

;;;###autoload
(defun narrow-to-defun-indirect-buffer ()
  (interactive)
  (narrow-to-indirect-buffer 'narrow-to-defun "Function"))

;;;###autoload
(defun narrow-to-region-indirect-buffer ()
  (interactive)
  (narrow-to-indirect-buffer 'narrow-to-region "Region"))
