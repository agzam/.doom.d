;;; custom/shell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun shell-pop-choose (&optional arg)
  (interactive "P")
  (let* ((shell-type (completing-read "Shell: " '(eshell vterm shell)))
         (shell-fn (pcase shell-type
                     ("eshell" #'eshell)
                     ("vterm" (lambda () (vterm)))
                     ("shell" #'shell))))
    (shell-pop--set-shell-type
     'shell-pop-shell-type
     `(,shell-type
       ,(format "*%s*" shell-type)
       (lambda () (,shell-fn))))
    (shell-pop arg)))


;;;###autoload
(defun shell-pop-in-project-root (&optional arg)
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell-pop arg)))

;;;###autoload
(defun eshell-clear+ ()
  (interactive)
  (eshell/clear-scrollback)
  (eshell-send-input))
