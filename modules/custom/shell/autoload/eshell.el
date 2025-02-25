;;; custom/shell/eshell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun eshell/b (buf-or-regexp)
  "Output buffer content of buffer matching BUF-OR-REGEXP."
  (let ((buf (if (bufferp buf-or-regexp)
                 buf-or-regexp
               (cl-loop for b in (buffer-list)
                        thereis (and (string-match-p
                                      buf-or-regexp (buffer-name b))
                                     b)))))
    (when buf
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
  (eshell/cd (+zoxide-find regexp)))

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
(defun eshell-send-detached-input (&optional arg)
  "Send current eshell input to a compilation buffer.
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


;;;###autoload
(defun eshell-send-detached-input-to-kitty (&optional arg)
  "Send current eshell input to the existing Kitty instance.
With an argument - keep the window open; with two - keep focus in Emacs.

Make sure remote control is enabled in kitty.conf:
https://sw.kovidgoyal.net/kitty/remote-control/
allow_remote_control yes
listen_on unix:/tmp/kitty_sock"
  (interactive "p")
  (let* ((cmd (buffer-substring eshell-last-output-end (point-max)))
         (ksock (car-safe (file-expand-wildcards "/tmp/kitty_sock-[0-9]*")))
         (keep-tab? (eq arg 4))
         (no-focus? (eq arg 16))
         (tmpf (expand-file-name
                (concat "kitty-run-" (format-time-string "%Y-%m-%d-%H%M%S"))
                (make-temp-file nil t))))
    (start-process-shell-command
     "detach-kitty" "*detached-kitty*"
     (format
      (concat
       "kitten @ --to unix:%1$s "
       "launch --type=tab "
       "--cwd '%3$s' "
       (when keep-tab? "--hold ")
       "/bin/zsh -i -c '%2$s "
       (unless keep-tab? (concat " 2>&1 | tee " tmpf))
       (when keep-tab? "; exec zsh")
       "' "
       (unless no-focus? "&& kitten @ --to unix:%1$s focus-window"))
      ksock
      cmd
      (eshell/pwd)))
    (eshell-add-input-to-history cmd)
    (eshell-reset)
    (unless keep-tab?
      (with-current-buffer (find-file-noselect tmpf)
        (compilation-mode)
        (auto-revert-mode)
        (display-buffer (current-buffer))))))
