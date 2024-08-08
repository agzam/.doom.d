;;; custom/web-browsing/autoload/misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +process-external-url (url)
  "To be called when an external process sends a URL to Emacs."
  (pcase url
    ((pred (string-match-p "https\\:\\/\\/www.youtube.com\\/watch"))
     (elfeed-tube-fetch url))
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
