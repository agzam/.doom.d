;;; custom/web-browsing/autoload/misc.el -*- lexical-binding: t; -*-

(defun +process-external-url (url)
  "To be called when an external process sends a URL to Emacs."
  (pcase url
    ((pred (string-match-p "https\\:\\/\\/www.youtube.com\\/watch"))
     (youtube-sub-extractor-extract-subs url))
    ((pred (and (string-match-p "https\\:\\/\\/github.com.*\\/blob" url)
                (modulep! :custom git)))
     (+fetch-github-raw-file url))
    (_
     (+eww-open-in-other-window url))))

(string-match-p
 "https\\:\\/\\/github.com.*\\/blob"
 "https://github.com/advthreat/iroh/pulls")
