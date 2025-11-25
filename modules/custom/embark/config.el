;;; custom/embark/config.el -*- lexical-binding: t; -*-

(after! embark
  (require 'org)
  (setq embark-cycle-key "C-;"
        embark-help-key "M-h"
        embark-confirm-act-all nil
        embark-quit-after-action t)

  (setq embark-indicators '(embark-which-key-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  (defadvice! embark-select-next-line-a (orig-fn &rest args)
    "embark-select always moves to the next item upon selection."
    :around #'embark-select
    (apply orig-fn args)
    (when (minibufferp)
      (vertico-next)))

  (defcustom +embark-url-config
    '((nil :actions (("b e" . +eww-open-in-other-window)
                     ("b o" . +browse-url)
                     ("c m" .  +link-plain->link-markdown)
                     ("c o" . +link-plain->link-org-mode)
                     ("RET" . +eww-open-in-other-window)))
      (yt-video
       :pattern "\\(youtube\\.com/watch\\|youtu\\.be/\\)"
       :actions (("b b" . mpv-open+)
                 ("RET" . mpv-open+)
                 ("b t" . youtube-sub-extractor-extract-subs)))
      (github-repo
       :pattern "github\\.com/[^/]+/[^/]+/?$"
       :actions (("b b" . forge-visit-topic-via-url)
                 ("RET" . forge-visit-topic-via-url)
                 ("c s" . git-https-url->ssh)
                 ("g c" . magit-clone-regular+)))
      (github-pulls
       :pattern "github\\.com/[^/]+/[^/]+/pulls\\(?:\\?.*\\)?$"
       :actions (("b b" . +forge-browse-topics)
                 ("RET" . +forge-browse-topics)))
      (github-issues
       :pattern "github\\.com/[^/]+/[^/]+/issues\\(?:\\?.*\\)?$"
       :actions (("b b" . +forge-browse-topics)
                 ("RET" . +forge-browse-topics)))
      (github-pr
       :pattern "github\\.com/[^/]+/[^/]+/pull/[0-9]+"
       :actions (("b b" . forge-visit-topic-via-url)
                 ("RET" . forge-visit-topic-via-url)
                 ("c b" . +link-plain->link-bug-reference)
                 ("g c" . magit-clone-regular+)))
      (github-issue
       :pattern "github\\.com/[^/]+/[^/]+/issues/[0-9]+"
       :actions (("b b" . forge-visit-topic-via-url)
                 ("RET" . forge-visit-topic-via-url)
                 ("c b" . +link-plain->link-bug-reference)
                 ("g c" . magit-clone-regular+)))
      (github-file
       :pattern "github\\.com/[^/]+/[^/]+/blob/[^/]+/.+"
       :actions (("b b" . +fetch-github-raw-file)
                 ("RET" . +fetch-github-raw-file)))
      (github-compare-link
       :pattern "github\\.com/[^/]+/[^/]+/compare/.+"
       :actions ())
      (github-commit
       :pattern "github\\.com/[^/]+/[^/]+/commit/[0-9a-f]+"
       :actions ())
      (reddit-link
       :pattern "https\\:\\/\\/www.reddit.com\\/.*"
       :actions (("b b" . reddigg-view-comments)
                 ("RET" . reddigg-view-comments)))
      (hackernews-link
       :pattern "https\\:\\/\\/news.ycombinator.com\\/.*"
       :actions (("b b" . hnreader-comment)
                 ("RET" . hnreader-comment)))
      (circle-ci-log
       :pattern "https\\:\\/\\/circleci.com\\/api\\/.*"
       :actions (("b b" . open-circleci-log)
                 ("RET" . open-circleci-log))))
    "Complete url configuration with patterns and actions."
    :type '(alist :key-type (choice (const nil) symbol)
            :value-type plist)
    :group 'embark-url-config)

  (+embark-setup-url-types)

  (add-to-list 'embark-target-finders '+embark-target-org-block)

  (defvar-keymap embark-org-block-map
    :doc "Embark actions for org blocks"
    :parent embark-general-map)
  (add-to-list 'embark-keymap-alist '(org-block . embark-org-block-map))

  (map!
   :after embark
   (:map embark-general-map
         "C-<return>" #'embark-dwim
         "m" #'embark-select
         "/" #'+embark-project-search
         (:prefix
          ("x" . "text")
          "p" #'awesome-switch-to-prev-app-and-type))
   (:map embark-file-map
         "x" #'embark-open-externally+
         "o" nil
         (:prefix ("o" . "open")
                  "j" (embark-split-action find-file +evil/window-split-and-follow)
                  "l" (embark-split-action find-file +evil/window-vsplit-and-follow)
                  "h" (embark-split-action find-file split-window-horizontally)
                  "k" (embark-split-action find-file split-window-vertically)
                  "a" (embark-ace-action find-file)))

   (:map embark-org-block-map
         (:prefix ("c" . "convert")
                  "c" #'embark-org-block-convert-to-src
                  "e" #'embark-org-block-convert-to-example
                  "q" #'embark-org-block-convert-to-quote))

   (:map embark-command-map
         "h" #'helpful-command)

   (:map
    embark-buffer-map
    "o" nil
    (:prefix ("o" . "open")
             "j" (embark-split-action switch-to-buffer +evil/window-split-and-follow)
             "a" (embark-ace-action switch-to-buffer)))

   (:map
    embark-function-map
    "o" nil
    (:prefix ("d" . "definition")
             "j" (embark-split-action embark-find-definition +evil/window-split-and-follow)
             "l" (embark-split-action embark-find-definition +evil/window-vsplit-and-follow)
             "h" (embark-split-action embark-find-definition split-window-horizontally)
             "k" (embark-split-action embark-find-definition split-window-vertically)
             "a" (embark-ace-action embark-find-definition)))
   (:map
    embark-org-heading-map
    (:prefix ("r" . "roam")
     :desc "add ref" "u" #'roam-ref-add-for-active-tab))

   (:map
    embark-url-map
    "RET" #'+eww-open-in-other-window
    (:prefix
     ("b" . "browse")
     :desc "browser" "o" #'browse-url
     :desc "eww" "e" #'+eww-open-in-other-window)
    (:prefix
     ("c" . "convert")
     :desc "markdown link" "m" #'+link-plain->link-markdown
     :desc "org-mode link" "o" #'+link-plain->link-org-mode
     :desc "bug-reference" "b" #'+link-plain->link-bug-reference))

   (:map embark-markdown-link-map
         "b" (cmd! () (+browse-url (markdown-link-url)))
         "v" #'forge-visit-topic-via-url
         (:prefix
          ("c" . "convert")
          :desc "org-mode link" "o" #'+link-markdown->link-org-mode
          :desc "plain" "p" #'+link-markdown->link-plain
          :desc "strip" "s" #'+link-markdown->just-text
          :desc "bug-reference" "b" #'+link-markdown->link-bug-reference))

   (:map embark-org-link-map
         "b" #'org-open-at-point
         "V" #'+open-link-in-vlc
         "v" #'forge-visit-topic-via-url
         (:prefix
          ("c" . "convert")
          :desc "markdown link" "m" #'+link-org->link-markdown
          :desc "plain" "p" #'+link-org->link-plain
          :desc "strip" "s" #'+link-org->just-text
          :desc "bug-reference" "b" #'+link-org->link-bug-reference
          :desc "roam heading" "r" #'+link-org->roam-heading))

   (:map embark-bug-reference-link-map
         "v" #'forge-visit-topic-via-url
         (:prefix ("b" . "browse")
          :desc "browser" "o" #'bug-reference-push-button
          :desc  "forge-visit" "b"  #'forge-visit-topic-via-url)
         (:prefix
          ("c" . "convert")
          :desc "markdown link" "m" #'+link-bug-reference->link-markdown
          :desc "org-mode link" "o" #'+link-bug-reference->link-org-mode
          :desc "plain" "p" #'+link-bug-reference->link-plain))

   (:map embark-rfc-number-map
    :desc "browse" "b" #'+browse-rfc-number-at-point)

   (:map
    embark-collect-mode-map
    :n "[" #'embark-previous-symbol
    :n "]" #'embark-next-symbol
    :n "TAB" #'+embark-collect-outline-cycle
    :n "m" #'embark-select)

   (:map
    (embark-command-map embark-symbol-map)
    (:after edebug
            (:prefix ("D" . "debug")
                     "f" #'+edebug-instrument-symbol
                     "F" #'edebug-remove-instrumentation)))
   (:map embark-region-map
         ;; otherwise, this shit opens another instance of Emacs
         "b" (cmd! (browse-url (buffer-substring-no-properties (region-beginning) (region-end)))))
   (:map
    (embark-identifier-map
     embark-region-map
     embark-sentence-map
     embark-paragraph-map)
    (:prefix
     ("x" . "text")
     (:when (modulep! :custom writing)
       (:prefix ("l" . "language")
        :desc "define" "d" #'define-it-at-point
        :desc "sdcv" "l" #'sdcv-search-pointer
        :desc "Merriam Webster" "m" #'mw-thesaurus-lookup-dwim
        :desc "wiktionary" "w" #'wiktionary-bro-dwim)
       (:prefix ("g" . "translate")
        :desc "en->ru" "e" #'google-translate-query-translate-reverse
        :desc "ru->en" "r" #'google-translate-query-translate
        :desc "es->en" "s" #'+google-translate-es->en
        :desc "en->es" "S" #'+google-translate-en->es
        :desc "translate" "g" #'google-translate-at-point)))))

  (add-hook! 'embark-collect-mode-hook
    (defun visual-line-mode-off-h ()
      (visual-line-mode -1)))

  ;; don't ask when killing buffers
  (setq embark-pre-action-hooks
        (cl-remove
         '(kill-buffer embark--confirm)
         embark-pre-action-hooks :test #'equal))

  (defadvice! embark-prev-next-recenter-a ()
    :after #'embark-previous-symbol
    :after #'embark-next-symbol
    (recenter))

  (dolist (finder '(+embark-target-markdown-link-at-point
                    +embark-target-bug-reference-link-at-point
                    +embark-target-RFC-number-at-point))
    (add-to-list 'embark-target-finders finder))

  (defvar-keymap embark-markdown-link-map
    :doc "Keymap for Embark markdown link actions."
    :parent embark-general-map)
  (add-to-list
   'embark-keymap-alist
   '(markdown-link embark-markdown-link-map))

  (defvar-keymap embark-bug-reference-link-map
    :doc "Keymap for Embark bug-reference link actions."
    :parent embark-general-map)
  (add-to-list
   'embark-keymap-alist
   '(bug-reference-link embark-bug-reference-link-map))

  (defvar-keymap embark-rfc-number-map
    :doc "Keymap for Embark RFC number link actions."
    :parent embark-general-map)
  (add-to-list
   'embark-keymap-alist
   '(rfc-number embark-rfc-number-map)))
