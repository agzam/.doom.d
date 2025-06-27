;;; custom/web-browsing/autoload/consult-omni.el -*- lexical-binding: t; -*-

;;;###autoload
(defun consult-omni--set-api-keys ()
  "Read list of keys and set for corresponding consult-omni-source a
function that retrieves the API key from ~/.authinfo.gpg.

It's safer to use a function rather than the concrete value of a key"
  (require 'dash)
  (cl-labels ((split-col (x) (split-string x ":")))
    (let ((keys-list
           `((consult-omni-brave-api-key "api.search.brave.com")
             (consult-omni-scopus-api-key "api.elsevier.com")
             (consult-omni-youtube-search-key "youtube-api")
             ;; googleapis.com record has two, colon separated parts:
             ;; custom search ID and the API key
             (consult-omni-google-customsearch-cx
              "www.googleapis.com" ,(-compose #'car #'split-col))
             (consult-omni-google-customsearch-key
              "www.googleapis.com" ,(-compose #'cadr #'split-col)))))
      (dolist (k keys-list)
        (let ((key (car k))
              (host (nth 1 k))
              (fn (nth 2 k))
              (auth-source-debug nil))
          (set (intern (symbol-name key))
               (lambda ()
                 (when-let ((ps
                             (with-temp-message ""
                              (auth-source-pick-first-password :host host))))
                   (if fn (funcall fn ps)
                     ps)))))))))

;;;###autoload
(defun consult-omni-load-sources+ ()
  (dolist (m '(consult-omni-brave
               consult-omni-browser-history
               consult-omni-duckduckgo
               consult-omni-elfeed
               consult-omni-gh
               consult-omni-google
               consult-omni-gptel
               consult-omni-invidious
               consult-omni-line-multi
               consult-omni-notmuch
               consult-omni-wikipedia
               consult-omni-youtube
               consult-omni-apps))
    (require m nil t)))

(defun search-in-slack ()
  (interactive)
  (when-let* ((search-term
               (read-string "Search in Slack: "
                            (if (use-region-p)
                                (buffer-substring (region-beginning) (region-end))
                              (symbol-name (symbol-at-point))))))
    (spacehammer--hs-eval-fennel
     (format "(do (local slack (require :my-slack)) (slack.search \"%s\"))"
             search-term))))

;;;###autoload
(transient-define-prefix consult-omni-transient ()
  ["consult-omni"
   [("/" "multi" consult-omni-multi)
    ("go" "google" consult-omni-google)
    ("w" "wiki" consult-omni-wikipedia)
    ("y" "youtube" consult-omni-youtube)]
   [("bh" "browser-hist" consult-omni-browser-history)
    ("el" "elfeed" consult-omni-elfeed)
    ("no" "notmuch" consult-omni-notmuch)
    ("gp" "gptel" consult-omni-gptel :if (lambda () (featurep 'gptel)))
    ("gf" "gptel log find" gptel-log-find :if (lambda () (featurep 'gptel)))]
   [("a" "apps" consult-omni-apps)
    ("gh" "code search" +search-github-with-lang)
    ("gH" "github" consult-omni-github :if (lambda () (featurep 'consult-gh)))
    ("ss" "slack search" search-in-slack :if (lambda () (featurep :system 'macos)))
    ("hn" "HN" consult-hn-transient :if (lambda () (featurep 'consult-hn)))]])

(defadvice! consult-omni-transient-a (&rest _)
  :before #'consult-omni-transient
  (consult-omni-load-sources+)
  (consult-omni--set-api-keys))
