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

;;;###autoload
(defun diff-current-buffer-with-file ()
  (interactive)
  (let ((buf (current-buffer)))
    (with-current-buffer buf
     (if buffer-file-name
         (progn
          (diff-buffer-with-file buf)
          (select-window (get-buffer-window "*Diff*")))
       (message "Buffer has no file!")))))

;;;###autoload
(defun ibuffer-sidebar-jump ()
  "Find ibuffer-sidebar window and jump to the line listing current buffer."
  (interactive)
  (if (member major-mode '(ibuffer-sidebar-mode ibuffer-mode))
      (call-interactively #'ibuffer-visit-buffer)
   (let ((name (buffer-name))
         (ibuffer-jump-offer-only-visible-buffers nil)
         (ibuffer-sidebar-pop-to-sidebar-on-toggle-open t))
     (if (ibuffer-sidebar-showing-sidebar-p)
         (pop-to-buffer (ibuffer-sidebar-get-or-create-buffer))
       (ibuffer-sidebar-toggle-sidebar))
     (when (string-match-p "*" name)
       (ibuffer-filter-disable))
     (ibuffer-jump-to-buffer name))))
