;;; custom/elisp/autoload/messages.el -*- lexical-binding: t; -*-

;;;###autoload
(defun erase-messages-buffer ()
  (interactive)
  (with-current-buffer "*Messages*"
    (read-only-mode -1)
    (erase-buffer)
    (read-only-mode 1)))

;;;###autoload
(defun +switch-to-messages-buffer-other-window ()
  (interactive)
  (setq +last-known-elisp-buffer (current-buffer))
  (if-let ((mwin (get-buffer-window (messages-buffer))))
      (select-window mwin)
    (progn
      (+evil/window-vsplit-and-follow)
      (switch-to-messages-buffer))))

;;;###autoload
(defun +switch-to-last-elisp-buffer ()
  (interactive)
  (when (boundp '+last-known-elisp-buffer)
    (select-window
     (get-buffer-window
      +last-known-elisp-buffer))))

;;;###autoload
(defun +hide-messages-window ()
  (interactive)
  (when-let ((mw (get-buffer-window (messages-buffer))))
    (delete-window mw)))
