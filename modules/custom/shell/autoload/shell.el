;;; custom/shell/autoload/shell.el -*- lexical-binding: t; -*-

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
  (if-let ((pr (projectile-project-root)))
      (projectile-with-default-dir pr
          (shell-pop arg))
    (shell-pop arg)))
