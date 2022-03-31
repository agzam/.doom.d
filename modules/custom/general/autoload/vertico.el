;;; custom/general/autoload/vertico.el -*- lexical-binding: t; -*-

(defvar vertico-posframe-global nil)

;;;###autoload
(defun vertico-posframe-briefly-off ()
  "When posframe obstructing text in the window, it's useful to
temporarily toggle it off. Bind in vertico-map."
  (interactive)
  (setq vertico-posframe-global vertico-posframe-mode)
  (when vertico-posframe-mode
    (posframe-delete-frame vertico-posframe--buffer)
    (vertico-posframe-mode -1)
    (run-at-time 0 nil #'abort-recursive-edit)
    (run-at-time
     0.01 nil
     (lambda ()
       (add-hook! 'vertico-posframe-mode-hook
         (defun remove-vertico-on-minibuffer-exit ()
           (remove-hook! 'minibuffer-exit-hook #'vertico-posframe-mode)))
       (add-hook 'minibuffer-exit-hook #'vertico-posframe-mode)
       (vertico-repeat-last)))))

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

;;;###autoload
(defun vertico-jump-to-home-dir-on~  ()
  (interactive)
  (if (and (vertico-directory--completing-file-p)
           (string-match-p "^~\\/" (minibuffer-contents)))
      (while (not (string-equal (minibuffer-contents) "~/"))
        (vertico-directory-up 1))
    (insert "~")))
