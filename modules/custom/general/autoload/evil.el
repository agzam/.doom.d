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