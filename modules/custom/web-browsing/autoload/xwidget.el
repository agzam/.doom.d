;;; custom/web-browsing/autoload/xwidget.el -*- lexical-binding: t; -*-

;;;###autoload
(defun xwidget-webkit-get-url-buffer (url)
  "Returns xwidget buffer that points to URL, nil if none."
  (interactive)
  (when-let* ((r (lambda (x)
                   (when x (replace-regexp-in-string "http\\(s\\|\\)://" "" x))))
              (fnd (seq-find
                    (lambda (x)
                      (string= (concat (funcall r url) "/")
                               (funcall r (xwidget-webkit-uri x))))
                    xwidget-list)))
    (xwidget-buffer fnd)))

;;;###autoload
(defun xwidget-webkit-url-get-create (url &optional buffer-name)
  "Opens existing xwidget buffer, if it exists for the given URL,
or creates new session. Optionally, BUFFER-NAME can be set"
  (interactive (list (or (thing-at-point 'url)
                         (car (browse-url-interactive-arg "xwidget url: ")))))
  (require 'xwidget)
  (or (xwidget-webkit-get-url-buffer url)
      (progn (xwidget-webkit-browse-url url :new-session)
             (let ((buf xwidget-webkit-last-session-buffer))
               (run-with-timer
                1 nil
                (lambda (buf buffer-name)
                  (with-current-buffer buf
                    (rename-buffer (or buffer-name (concat "*xwidget " url "*")))))
                buf buffer-name)
               buf))))

;;;###autoload
(defun kill-all-xwidget-buffers ()
  "Kill all xwidget buffers without asking any questions. Useful to execute when Emacs gets stuck."
  (interactive)
  (let ((blist (seq-map 'xwidget-buffer xwidget-list))
        (kill-buffer-query-functions nil))
   (thread-last
     blist
     (seq-map #'get-buffer-window)
     (seq-remove #'null)
     (seq-do (fn! (w) (quit-window :kill w))))
   (seq-do #'kill-buffer blist)))
