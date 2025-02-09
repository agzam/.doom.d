;;; custom/web-browsing/autoload/misc.el -*- lexical-binding: t; -*-
(require 'bug-reference)

;;;###autoload
(defun +process-external-url (&optional url)
  "To be called when an external process sends a URL to Emacs."
  (interactive (list (read-string "Enter URL: ")))
  (pcase url
    ((pred (string-match-p bug-reference-bug-regexp))
     (cl-letf (((symbol-function 'browse-url) #'forge-visit-topic-via-url))
       (call-interactively #'bug-reference-push-button)))

    ((pred (string-match-p "https\\:\\/\\/news.ycombinator.com\\/.*"))
     (hnreader-comment url))

    ((pred (string-match-p "https\\:\\/\\/www.reddit.com\\/.*"))
     (reddigg-view-comments url))

    ((pred (string-match-p "https\\:\\/\\/www.youtube.com\\/watch.*"))
     (progn
       (message "opening %s" url)
       (mpv-transient)
       (mpv-play-url url)))

    ((pred (and (string-match-p "https\\:\\/\\/github.com.*" url)
                (modulep! :custom git)))
     (forge-visit-topic-via-url url))

    (_
     (+eww-open-in-other-window url))))

;;;###autoload
(defun +browse-url (url &rest args)
  "Always use default (external) browser"
  (interactive (browse-url-interactive-arg "URL: "))
  ;; eww resets browse-url-function, I don't want that
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (funcall-interactively #'browse-url url args)))
