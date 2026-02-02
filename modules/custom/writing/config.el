;;; custom/writing/config.el -*- lexical-binding: t; -*-
(use-package! spacehammer
  :defer t
  :commands spacehammer-edit-with-emacs
  :config
  (add-hook! 'spacehammer-edit-with-emacs-hook
             #'spacehammer-edit-with-emacs-h)
  (add-hook! 'spacehammer-before-finish-edit-with-emacs-hook
             #'spacehammer-before-finish-edit-with-emacs-h)

  (add-to-list
   'display-buffer-alist
   '("\\* spacehammer-edit.*"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(use-package! mw-thesaurus
  :defer t
  :commands mw-thesaurus-lookup-dwim
  ;; :hook (mw-thesaurus-mode . variable-pitch-mode)
  :config
  (map! :map mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
  (add-to-list
   'display-buffer-alist
   `(,mw-thesaurus-buffer-name
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.3))))

(use-package! sdcv
  :defer t
  :commands (sdcv-search-pointer sdcv-search)
  :hook (sdcv-mode . visual-line-mode)
  :config
  (map! :map sdcv-mode-map
        :n "q" #'sdcv-quit
        :n "n" #'sdcv-next-dictionary
        :n "p" #'sdcv-previous-dictionary
        :n "TAB" #'outline-cycle-buffer
        :n "<backtab>" #'outline-show-all
        :ni "RET" #'sdcv-search-pointer
        :n "a" #'sdcv-search-at-point)
  (setq sdcv-word-pronounce nil)
  (add-to-list
   'display-buffer-alist
   `(,sdcv-buffer-name
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(use-package! google-translate
  :defer t
  :init
  (require 'google-translate)
  :functions (google-translate--search-tkk)
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (setq google-translate-pop-up-buffer-set-focus t
        google-translate-default-source-language "auto"
        google-translate-default-target-language "en")

  (setopt google-translate-listen-program (executable-find "ffplay")
          google-translate-listen-program-args '("-nodisp" "-autoexit" "-loglevel" "quiet"))
  (setq google-translate-input-method-auto-toggling t
        google-translate-preferable-input-methods-alist
        '((nil . ("en"))
          (spanish-prefix . ("es"))
          (russian-computer . ("ru"))))

  (add-hook! 'google-translate-mode-hook
    (defun google-translate-mode-h ()
      (variable-pitch-mode +1)
      (pop-to-buffer "*Google Translate*")
      (map! :map google-translate-mode-map
            (:localleader
             "l" #'google-translate-listen-source+
             "L" #'google-translate-listen-translation+))))

  (add-to-list
   'display-buffer-alist
   '("\\*Google Translate\\*"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (init-width . 0.3)
     (window . root))))

(use-package! define-it
  :defer t
  :commands define-it-at-point
  :config
  (setq
   define-it-show-google-translate nil
   define-it-show-header nil)

  ;; it doesn't pop to the buffer automatically, when definition is fetched
  (defun define-it--find-buffer (x)
    (let ((buf (format define-it--buffer-name-format define-it--current-word)))
      (pop-to-buffer buf)))

  (advice-add 'define-it--in-buffer :after #'define-it--find-buffer)
  (add-to-list
   'display-buffer-alist
   '("\\*define-it:"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(use-package! separedit
  :defer t
  :commands separedit separedit-dwim
  :init
  (map! :map prog-mode-map :inv "C-c '"
        (cmd! () (cond
                  ((bound-and-true-p org-src-mode) (org-edit-src-exit))
                  ((eq major-mode 'separedit-double-quote-string-mode) (separedit-commit))
                  (t (separedit-dwim)))))
  (map! :map (separedit-double-quote-string-mode-map
              separedit-single-quote-string-mode-map)
        :inv "C-c '" #'separedit-commit)
  (map! :map minibuffer-local-map "C-c '" #'separedit)
  :config
  (setq separedit-default-mode 'markdown-mode))

(after! ispell
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (setq ispell-program-name "enchant-2")
  (add-to-list 'ispell-dictionary-alist
               '(nil "[[:alpha:]]"
                 "[^[:alpha:]]"
                 "['’]" nil ("-B") nil utf-8))
  ;; (unless ispell-dictionary-alist
  ;;   (setq ispell-dictionary-alist
  ;;         '(("american"   ;; English
  ;;            "[A-Za-z]" "[^A-Za-z]" "[']" nil
  ;;            ("-B" "-d" "en_US")
  ;;            nil iso-8859-1)
  ;;           ("russian" "[\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]" "[^\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]" "" nil nil nil koi8-r))))

  ;; (setq
  ;;  ;; ispell-extra-args '("--sug-mode=ultra" "--run-together")
  ;;  ispell-aspell-dictionary-alist ispell-dictionary-alist
  ;;  ispell-dictionary "american"
  ;;  )

  (defadvice! change-dict-after-toggle-input (fn &optional arg interactive)
    :around #'toggle-input-method
    :around #'set-input-method
    (funcall fn arg interactive)
    (let ((dic+lan (pcase current-input-method
                     ("russian-computer" '("ru_RU" "russian"))
                     ((or "spanish-keyboard"
                          "spanish-prefix"
                          "spanish-postfix")
                      '("es_MX" "spanish"))
                     (_ '("en_US" "american-english")))))
      (setq ispell-alternate-dictionary
            (format "/usr/share/dict/%s" (cadr dic+lan)))
      (ispell-change-dictionary (car dic+lan)))))

(after! quail
  (quail-define-package
   "Emoji" "UTF-8" "😎" t
   "Emoji input mode for people that really, really like Emoji"
   '(("\t" . quail-completion))
   t t nil nil nil nil nil nil nil t)
  (quail-define-rules

   (":)" ?😀)
   (":(" ?😕)
   (":P" ?😋)
   (":D" ?😂)
   (":party:" ?🎉)
   (":spock:" ?🖖)
   (":thumb:" ?👍)))

(after! writegood-mode
  (remove-hook! 'org-mode-hook #'writegood-mode))

(after! grip-mode
  (setq grip-preview-use-webkit (featurep :system 'macos))
  (setq grip-github-user "agzam")
  (setf grip-github-password (auth-host->pass "api.github.com")))

(after! markdown-mode
  (setopt markdown-enable-math nil)
  (require 'lsp-marksman)
  (map! :map (markdown-mode-map)
        (:localleader
         (:prefix ("s" . "wrap")
                  "<" #'markdown-wrap-collapsible
                  "C" #'markdown-wrap-code-clojure
                  "c" #'markdown-wrap-code-generic)
         (:prefix ("o" . "open/links")
                  "o" #'markdown-open
                  "l" #'markdown-store-link))
        :i "[[" #'markdown-insert-link+
        :i "[ SPC" (cmd! (insert "[]" (backward-char)))))

(use-package! youtube-sub-extractor
  :commands (youtube-sub-extractor-extract-subs)
  :config
  (map! :map youtube-sub-extractor-subtitles-mode-map
        :desc "copy timestamp URL" :n "RET" #'youtube-sub-extractor-copy-ts-link
        :desc "browse at timestamp" :n "C-c C-o" #'youtube-sub-extractor-browse-ts-link
        :n "q" #'kill-buffer-and-window))

(use-package! wiktionary-bro
  :commands (wiktionary-bro-dwim)
  :config
  (when (and (featurep 'evil))
    (dolist (state '(normal visual insert))
      (evil-make-intercept-map
       (evil-get-auxiliary-keymap wiktionary-bro-mode-map state t t)
       state)))

  (map! :map wiktionary-bro-mode-map
        ;; :nvi  "<return>" #'wiktionary-bro-dwim
        :n "q" #'kill-current-buffer)

  (add-hook! 'wiktionary-bro-mode-hook
    (defun jinx-mode-off () (jinx-mode -1))
    (defun wiktionary-bro-use-eww-open-in-other-window-h ()
      (setq-local browse-url-browser-function #'+eww-open-in-other-window)))

  (add-to-list
   'display-buffer-alist
   '((major-mode . wiktionary-bro-mode)
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (init-width . 0.30)
     (window . root))))

(use-package! jinx
  :defer t
  :hook (doom-first-buffer . global-jinx-mode)
  :config
  (after! vertico-multiform
    (add-to-list 'vertico-multiform-categories
                 '(jinx grid (vertico-grid-annotate . 20)))
    (vertico-multiform-mode 1))

  (setq jinx-languages "en_US ru_RU es_MX")

  (map! :map jinx-mode-map
        :i ", SPC" #'insert-comma
        :i ",," #'jinx-autocorrect-last+
        :i ",." (cmd! (jinx-autocorrect-last+ :prompt))))

(use-package! translate-popup
  :after (google-translate))

(defadvice! forward-paragraph-fix-a (&rest _)
  "Move to first character of paragraph not the space before it"
  :after #'forward-paragraph
  (when (called-interactively-p 'any)
    (skip-chars-forward " \t\n")))

(defadvice! backward-paragraph-fix-a (ofn &rest _)
  "Move to first character of paragraph not the space after it"
  :around #'backward-paragraph
  (if (and (called-interactively-p 'any))
      (progn
        (skip-chars-backward " \n")
        (funcall ofn)
        (when (looking-at-p "[[:blank:]]*$")
          (skip-chars-forward " \t\n")))
    (funcall ofn)))

(defadvice! recenter-on-paragraph-a (&rest _)
  :after #'forward-paragraph
  :after #'backward-paragraph
  (when (called-interactively-p 'any)
    (recenter)
    (pulse-momentary-highlight-one-line)))

(defadvice! flash-on-recenter-a (&rest _)
  :after #'recenter-top-bottom
  (when (called-interactively-p 'any)
    (pulse-momentary-highlight-one-line)))
