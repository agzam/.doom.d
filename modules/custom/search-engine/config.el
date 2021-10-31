;;; custom/search-engine/config.el -*- lexical-binding: t; -*-

(use-package! engine-mode
  :init
  (progn
    (defvar search-engine-config-list nil
      "Set additional search engines")

    (setq search-engine-alist
          `((amazon
             :name "Amazon"
             :url (concat "https://www.amazon."
                          search-engine-amazon-tld
                          "/s/ref=nb_sb_noss?url=search-alias%%3Daps&field-keywords=%s"))
            (duck-duck-go
             :name "Duck Duck Go"
             :url "https://duckduckgo.com/?q=%s")
            (google
             :name "Google"
             :url "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
            (google-images
             :name "Google Images"
             :url "https://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")
            (github
             :name "GitHub"
             :url "https://github.com/search?ref=simplesearch&q=%s")
            (google-maps
             :name "Google Maps"
             :url "https://maps.google.com/maps?q=%s")
            (twitter
             :name "Twitter"
             :url "https://twitter.com/search?q=%s")
            (youtube
             :name "YouTube"
             :url "https://www.youtube.com/results?aq=f&oq=&search_query=%s")
            (wikipedia
             :name "Wikipedia"
             :url "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            (maven
             :name "Maven Central"
             :url "https://search.maven.org/search?q=%s")
            (npm
             :name "Npmjs")
            (clojure
             :name "Clojure Docs"
             :url "https://clojuredocs.org/search?q=%s")
            (wolfram-alpha
             :name "Wolfram Alpha"
             :url "https://www.wolframalpha.com/input/?i=%s")
            ,@search-engine-config-list))
    (dolist (engine search-engine-alist)
      (let ((func (intern (format "engine/search-%S" (car engine)))))
        (autoload func "engine-mode" nil 'interactive))))

  :config
  (engine-mode +1)
  (dolist (engine search-engine-alist)
    (let* ((cur-engine (car engine))
           (engine-url (plist-get (cdr engine) :url))
           (engine-keywords (plist-get (cdr engine) :keywords)))
      (eval `(defengine ,cur-engine ,engine-url ,@engine-keywords))))

  (map! :leader
        "s/" #'engine/search-google
        "g/" #'engine/search-github-with-lang))
