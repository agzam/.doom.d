;;; custom/shell/eshell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun eshell/b (regexp)
  "Output buffer content of buffer matching REGEXP."
  (cl-loop for buf in (buffer-list)
           thereis
           (and (string-match-p regexp (buffer-name buf))
                (with-current-buffer buf
                  (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun eshell-clear+ ()
  (interactive)
  (eshell/clear-scrollback)
  (eshell-send-input))

(defun +eshell-buffer-contents (buffer)
  "Return fontified buffer contents for BUFFER."
  (with-current-buffer buffer
    (font-lock-ensure (point-min) (point-max))
    (buffer-string)))

(defun +eshell-file-contents (file)
  "Return fontified file contents for FILE."
  (let ((buffer (get-file-buffer file)))
    (if buffer
        (+eshell-buffer-contents buffer)
      (unwind-protect
          (+eshell-buffer-contents
           (setq buffer
                 (let ((inhibit-message t)
                       (non-essential t)
                       (enable-dir-local-variables nil)
                       (enable-local-variables (and enable-local-variables :safe)))
                   (find-file-noselect file))))
        (when buffer
          (kill-buffer buffer))))))

;;;###autoload
(defun eshell/hat (&rest files)
  "Output FILES with highlighting."
  (dolist (f files)
    (eshell-print (+eshell-file-contents f))))
