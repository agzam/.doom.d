;;; custom/spacemacsy/autoload/buffers.el -*- lexical-binding: t; -*-

(defun spacemacs/rudekill-matching-buffers (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns the count of killed buffers."
  (let* ((buffers (cl-remove-if-not
                   (lambda (buffer)
                     (let ((name (buffer-name buffer)))
                       (and name (not (string-equal name ""))
                            (or internal-too (/= (aref name 0) ?\s))
                            (string-match regexp name))))
                   (buffer-list))))
    (mapc 'kill-buffer buffers)
    (length buffers)))

;;;###autoload (autoload 'spacemacs/kill-matching-buffers-rudely "custom/spacemacsy/autoload/buffers" nil t)
(defun spacemacs/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the specified REGEXP. See
the `kill-matching-buffers` for grateful killing. The optional 2nd argument
indicates whether to kill internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message
   (format "%d buffer(s) killed."
           (spacemacs/rudekill-matching-buffers regexp internal-too))))

;;;###autoload (autoload 'alternate-buffer "custom/spacemacsy/autoload/buffers" nil t)
(defun alternate-buffer ()
  (interactive)
  (persp-add-buffer (current-buffer))
  (when-let ((b (evil-alternate-buffer (get-buffer-window))))
    (switch-to-buffer (car b))))

;;;###autoload (autoload 'toggle-maximize-buffer "custom/spacemacsy/autoload/buffers" nil t)
(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))
