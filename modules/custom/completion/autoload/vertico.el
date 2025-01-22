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
  ;; (print (pp (frame-parameters (car (frame-list-z-order)))))
  ;; (print (thread-last (frame-list)
  ;;                     (seq-do (lambda (x) (pp (frame-parameters x))))))
  (let* ((frame-alpha-lower-limit 0.0)
         (posframe (car (frame-list-z-order)))
         (alpha (frame-parameter posframe 'alpha))
         (new-alpha (if (eql alpha 1.0) 0.2 1.0)))
    ;; Emacs can't register key-released event, I have to do a trick with a timer
    (when (boundp 'vertico-posframe--reset-alpha-timer)
      (cancel-timer vertico-posframe--reset-alpha-timer))
    (modify-frame-parameters posframe `((alpha . ,new-alpha)))
    (setq vertico-posframe--reset-alpha-timer
          (run-with-timer
           5 nil (lambda () (modify-frame-parameters posframe `((alpha . 1.0))))))))

;;;###autoload
(defun vertico-posframe-briefly-tall ()
  (interactive)
  (setq vertico-posframe-height 60
        vertico-count 100)
  (add-hook! 'minibuffer-exit-hook
    (defun vertico-posframe-height-restore-h ()
      (setq vertico-posframe-height nil
            vertico-count 15))))

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
  "Puts you in home dir initial location in find-file"
  (interactive)
  (when (eq 'file (vertico--metadata-get 'category))
    (vertico-directory-up 1000)
    (unless (string-match-p "^~.*" (minibuffer-contents))
      (beginning-of-line)
      (insert "~")
      (end-of-line))))

;;;###autoload
(defun vertico-jump-root  ()
  "Puts you in root dir initial location in find-file"
  (interactive)
  (when (eq 'file (vertico--metadata-get 'category))
    (vertico-directory-up 1000)
    (vertico-directory-up 1000)))

;;;###autoload
(defun vertico-jump-sudo  ()
  "Puts you in /sudo::/ initial location in find-file"
  (interactive)
  (when (eq 'file (vertico--metadata-get 'category))
    (vertico-directory-up 1000)
    (vertico-directory-up 1000)
    (insert "sudo::/")
    (end-of-line)))
