;;; custom/completion/autoload/vertico.el -*- lexical-binding: t; -*-

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
       (vertico-repeat)))))

;;;###autoload
(defun vertico-posframe-briefly-transparent ()
 "When posframe obstructing text, you can temporarily make it
transparent. Bind in vertico-map."
  (interactive)
  (let* ((frame-alpha-lower-limit 10)
         (posframe (car (frame-list-z-order)))
         (alpha (frame-parameter posframe 'alpha))
         (new-alpha (if (eq alpha 100) 0 100)))
    ;; Emacs can't register key-released event, I have to do a trick with a timer
    (when (boundp 'vertico-posframe--reset-alpha-timer)
      (cancel-timer vertico-posframe--reset-alpha-timer))
    (modify-frame-parameters posframe `((alpha . ,new-alpha)))
    (setq vertico-posframe--reset-alpha-timer
          (run-with-timer
           3 nil (lambda () (modify-frame-parameters posframe `((alpha . 100))))))))

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
  (when (eq 'file (vertico--metadata-get 'category))
    (vertico-directory-up 1000)
    (when (string-blank-p (minibuffer-contents))
      (insert "~"))))
