;;; custom/general/autoload/buffers.el -*- lexical-binding: t; -*-

(defun spacemacs/rudekill-matching-buffers (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the
specified REGEXP. See the `kill-matching-buffers` for grateful
killing. The optional 2nd argument indicates whether to kill
internal buffers too.

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

;;;###autoload
(defun spacemacs/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill - WITHOUT ASKING - buffers whose name matches the
specified REGEXP. See the `kill-matching-buffers` for grateful
killing. The optional 2nd argument indicates whether to kill
internal buffers too.

Returns a message with the count of killed buffers."
  (interactive "sKill buffers matching this regular expression: \nP")
  (message
   (format "%d buffer(s) killed."
           (spacemacs/rudekill-matching-buffers regexp internal-too))))

;;;###autoload
(defun alternate-buffer ()
  (interactive)
  (when-let ((b (evil-alternate-buffer (get-buffer-window))))
    (switch-to-buffer (car b))))

;;;###autoload
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


;;;###autoload
(defun switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
if prefix argument ARG is given, switch to it in an other,
possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))
    (evil-normal-state)))

;;;###autoload
(defun kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the
window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))
