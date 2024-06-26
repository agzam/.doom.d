;;; custom/web-browsing/autoload/consult-web.el -*- lexical-binding: t; -*-

;;;###autoload
(defun consult-web--set-api-keys ()
  "Read list of keys and set for corresponding consult-web-source a
function that retrieves the API key from ~/.authinfo.gpg.

It's safer to use a function rather than the concrete value of a key"
  (cl-labels ((split-col (x) (split-string x ":")))
    (let ((keys-list
           `((consult-web-brave-api-key "api.search.brave.com")
             (consult-web-scopus-api-key "api.elsevier.com")
             (consult-web-youtube-search-key "youtube-api")
             ;; googleapis.com record has two, colon separated parts:
             ;; custom search ID and the API key
             (consult-web-google-customsearch-cx
              "www.googleapis.com" ,(-compose #'car #'split-col))
             (consult-web-google-customsearch-key
              "www.googleapis.com" ,(-compose #'cadr #'split-col)))))
      (dolist (k keys-list)
        (let ((key (car k))
              (host (nth 1 k))
              (fn (nth 2 k)))
          (set (intern (symbol-name key))
               (lambda ()
                 (when-let ((ps
                             (with-temp-message ""
                              (auth-source-pick-first-password :host host))))
                   (if fn (funcall fn ps)
                     ps)))))))))

;;;###autoload
(defun consult-web-multi-dwim ()
  (interactive)
  (let ((initial (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (word-at-point))))
    (consult-web-multi initial)))


;;;###autoload
(defun consult-web-load-sources+ ()
  (dolist (m '(consult-web-brave
               consult-web-browser-history
               consult-web-duckduckgo
               consult-web-elfeed
               consult-web-gh
               consult-web-google
               consult-web-gptel
               consult-web-invidious
               consult-web-line-multi
               consult-web-notmuch
               consult-web-wikipedia
               consult-web-youtube))
    (require m nil t)))

;;;###autoload
(transient-define-prefix consult-web-transient ()
  ["consult-web"
   [("/" "multi" consult-web-multi-dwim)
    ("go" "google" consult-web-google)
    ("w" "wiki" consult-web-wikipedia)
    ("y" "youtube" consult-web-youtube)
    ("gh" "github" consult-web-github)]
   [("bh" "browser-hist" consult-web-browser-history)
    ("el" "elfeed" consult-web-elfeed)
    ("no" "notmuch" consult-web-notmuch)
    ("gp" "gptel" consult-web-gptel)]])

(advice-add 'consult-web-transient :before #'consult-web-load-sources+)
