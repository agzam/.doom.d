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


(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell."
  (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                          (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                           :narrow ?e
                                           :category file
                                           :face consult-file
                                           :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
        (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))

;; From https://protesilaos.com/dotemacs
(defun eshell-export-output+ (&optional arg)
  "Export output of the last command to a buffer.
With prefix ARG, also copy the prompt and input."
  (interactive)
  (let ((orig (current-buffer))
        (beg (if arg (eshell-beginning-of-input)
               (eshell-beginning-of-output)))
        (end (eshell-end-of-output))
        (buffer (get-buffer-create
                 (format "*eshell export: %s*"
                         (buffer-substring-no-properties
                          (eshell-beginning-of-input)
                          (1- (eshell-beginning-of-output)))))))
    (with-current-buffer buffer
      (font-lock-mode)
      (insert-buffer-substring orig beg end)
      (goto-char (point-min)))
    ;; Taken from `eshell-kill-output'
    (goto-char (eshell-beginning-of-output))
    (insert (format "Exported to %S\n" buffer))
    (delete-region (point) (eshell-end-of-output))
    (goto-char (point-max))
    (pop-to-buffer buffer)))

;;From https://github.com/nbarrientos/dotfiles/.emacs.d/init.el
;;;###autoload
(defun eshell-send-detached-input+ (&optional arg)
  "Send the current Eshell input to a compilation buffer.
With universal prefix argument bury the compilation buffer and
send a notification when the process has exited."
  (interactive "p")
  (let* ((cmd (buffer-substring
               eshell-last-output-end (point-max)))
         (hostname (car (split-string
                         (or
                          (file-remote-p default-directory 'host)
                          (system-name))
                         "\\.")))
         (compile-command nil)
         (compilation-buffer-name-function
          (lambda (_major-mode)
            (format "D# %s (%s)" cmd hostname)))
         (compilation-buffer (compile cmd)))
    (when (equal arg 4)
      (with-current-buffer compilation-buffer
        (switch-to-prev-buffer (get-buffer-window (current-buffer)))
        (setq-local compilation-finish-functions
                    `((lambda (buffer str)
                        (notifications-notify
                         :body ,cmd
                         :timeout 8000
                         :category "detached_process"
                         :actions '("default" "Switch to buffer")
                         :on-action (lambda (id key) (switch-to-buffer-other-window ,(buffer-name compilation-buffer)))
                         :title (format "Process running in '%s' finished!" ,hostname)
                         :urgency (if (string-prefix-p "finished" str) 'normal 'critical)))))))
    (eshell-add-input-to-history cmd)
    (eshell-reset)))
