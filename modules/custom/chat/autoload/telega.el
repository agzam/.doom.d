;;; custom/chat/autoload/telega.el -*- lexical-binding: t; -*-

;;;###autoload
(defun telega-extract-and-attach-video (url)
  "Extracts video from URL and post into the current chat buffer."
  (interactive (list (read-string
                      "Video URL: "
                      (when-let* ((k (car-safe kill-ring))
                                (_ (string-match-p
                                    "^https?://" k)))
                        (string-trim k)))))
  (yt-extract-video-y-entonces
   url
   (lambda (fpath)
     (funcall-interactively #'telega-chatbuf-attach-video fpath)
     ;; TODO: figure that out (delete-file fpath)
     )))
