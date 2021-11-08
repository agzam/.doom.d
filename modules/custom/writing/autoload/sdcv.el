;;; custom/writing/autoload/sdcv.el -*- lexical-binding: t; -*-

(defun region-or-word-at-point-str ()
  "Returns string of selected region or word at point"
  (let* ((bds (if (use-region-p)
                  (cons (region-beginning) (region-end))
                (bounds-of-thing-at-point 'word)))
         (p1 (car bds))
         (p2 (cdr bds)))
    (buffer-substring-no-properties p1 p2)))

;;;###autoload (autoload 'sdcv-search-at-point "custom/writing/autoload/sdcv" nil t)
(defun sdcv-search-at-point ()
    (interactive)
    (sdcv-search (region-or-word-at-point-str) nil nil t))
