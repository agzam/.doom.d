;;; custom/writing/autoload/jinx.el -*- lexical-binding: t; -*-

(defun jinx--autocorrect-overlay (overlay &optional info)
  "Auto correct word at OVERLAY, maybe show prompt INFO."
  (catch 'jinx--goto
    (let* ((word (buffer-substring-no-properties
                  (overlay-start overlay) (overlay-end overlay)))
           (choice
            (jinx--correct-highlight overlay
              (lambda ()
                (when (or (< (point) (window-start)) (> (point) (window-end nil t)))
                  (recenter))
                (car (jinx--correct-suggestions word)))))
           (len (length choice)))
      (pcase (and (> len 0) (assq (aref choice 0) jinx--save-keys))
        (`(,key . ,fun)
         (funcall fun 'save key (if (> len 1) (substring choice 1) word))
         (jinx--recheck-overlays))
        ((guard (not (equal choice word)))
         (jinx--correct-replace overlay choice)))
      nil)))

;;;###autoload
(defun jinx-autocorrect-nearest+ ()
  "Auto-correct nearest misspelled word.
A dirty hack until there's a solution for https://github.com/minad/jinx/issues/100"
  (interactive "*")
  (save-excursion
    (jinx--correct-guard
     (let* ((overlays (jinx--force-overlays (window-start) (window-end) :visible t))
            (count (length overlays))
            (idx 0))
       ;; Not using `while-let' is intentional here.
       (while (when-let ((ov (nth idx overlays)))
                (if (overlay-buffer ov)
                    (when-let ((skip (jinx--autocorrect-overlay ov)))
                      (undo-boundary)
                      (setq idx (mod (+ idx skip) count)))
                  (cl-incf idx))))))))
