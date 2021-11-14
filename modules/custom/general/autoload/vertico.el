;;; custom/general/autoload/vertico.el -*- lexical-binding: t; -*-

(defvar vertico-posframe-global nil)

;;;###autoload
(defun vertico-posframe-temporarily-off ()
  "When posframe obstructing text in the window, it's useful to
temporarily toggle it off. Bind in vertico-map."
  (interactive)
  (setq vertico-posframe-global vertico-posframe-mode)
  (when (fboundp #'vertico-posframe-mode)
    (vertico-posframe-mode -1)))

;;;###autoload
(defun restore-vertico-posframe-state-h ()
  (when (not (eq vertico-posframe-mode
                 vertico-posframe-global))
    (run-at-time
     "0 sec" nil
     (fn! ()
       (when vertico-posframe-global
         (vertico-posframe-mode +1)
         (setq vertico-posframe-global nil))))))
