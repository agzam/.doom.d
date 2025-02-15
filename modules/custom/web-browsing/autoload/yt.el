;;; custom/web-browsing/autoload/yt.el -*- lexical-binding: t; -*-
(defvar yt-extracted-vids ()
  "The list of extracted videos,
where each item is a k/v pair of the url and filepath.")

(defun yt-extract-video-y-entonces (url &optional callback)
  "Extracts video from URL with yt-dlp and runs CALLBACK fn.

Passes the filepath as the param to CALLBACK."
  (interactive "sVideo URL: ")
  (let* ((default-directory "~/Movies/")
         (pbuf "*yt-dlp*")
         (process (async-shell-command
                   (format "yt-dlp --verbose --restrict-filenames '%s'" url) pbuf)))
    (set-process-sentinel
     (get-buffer-process pbuf)
     (lambda (process event)
       (cond ((string= event "finished\n")
              (when-let* ((fpath (with-current-buffer pbuf
                                   (goto-char (point-max))
                                   (when (re-search-backward
                                          "Deleting original file \\|\\[download\\] \\([^.]+\\)" nil t)
                                     (let ((base-name (match-string-no-properties 1)))
                                       (car-safe
                                        (directory-files
                                         (expand-file-name default-directory)
                                         'full
                                         (concat
                                          "^"
                                          (regexp-quote base-name) "\\."))))))))
                (setf (plist-get yt-extracted-vids url) fpath)
                (when callback (funcall callback fpath))))
             (t (message "downloading %s" url)))))))
