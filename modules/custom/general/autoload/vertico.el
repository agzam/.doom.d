;;; custom/general/autoload/vertico.el -*- lexical-binding: t; -*-

(defvar vertico-posframe-global nil)

;;;###autoload
(defun vertico-posframe-temporarily-off ()
  "When posframe obstructing text in the window, it's useful to
temporarily toggle it off. Bind in vertico-map."
  (interactive)
  (when (and (bound-and-true-p vertico-posframe-mode)
             (fboundp #'vertico-posframe-mode))
    (setq vertico-posframe-global vertico-posframe-mode)
    (vertico-posframe-mode -1)))

;;;###autoload
(defun restore-vertico-posframe-state-h ()
  (when (not (eq vertico-posframe-mode
                 vertico-posframe-global))
    (run-at-time
     "0.01 sec" nil
     (lambda () (vertico-posframe-mode vertico-posframe-global)))))
