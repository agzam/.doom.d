;;; custom/general/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun evil-ex-visual-star-search-a (fn unbounded direction count &optional symbol)
  "Visual-star search in evil.

I want * and # operators to respect marked region."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (1+ (region-end)))
             (delta (- end beg 1))
             (s (buffer-substring beg end))
             (evil-ex-search-pattern s)
             (evil-ex-search-offset 0))
        (deactivate-mark)
        (if (eq direction 'forward)
            (goto-char end)
          (goto-char beg))
        (when (evil-ex-search-full-pattern s count direction)
          (let ((p (progn (when (eq direction 'forward)
                            (backward-char))
                          (point))))
            (set-mark p)
            (goto-char (if (eq direction 'forward)
                           (- p delta)
                         (+ p delta))))))
    (funcall fn unbounded direction count symbol)))

(require 'evil)

;; I may not need to override these manually,
;; watch for PR doomemacs/doomemacs#7218
;;;###autoload
(defun +evil-window-move-right ()
  "Swap windows to the right"
  (interactive)
  (if (and (window-at-side-p nil 'right)
           (not (or (window-in-direction 'above)
                    (window-in-direction 'below))))
      (evil-window-move-far-left)
    (+evil--window-swap 'right)))

;;;###autoload
(defun +evil-window-move-left ()
  "Swap windows to the right"
  (interactive)
  (if (and (window-at-side-p nil 'left)
           (not (or (window-in-direction 'above)
                    (window-in-direction 'below))))
      (evil-window-move-far-right)
    (+evil--window-swap 'left)))
