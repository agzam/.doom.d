;;; custom/writing/config.el -*- lexical-binding: t; -*-
(map! :leader
      :prefix "x"
      (:prefix ("l" . "language")
       "d" #'define-it-at-point
       "g" #'lsp-grammarly-check-grammar
       "l" #'sdcv-search-at-point
       "m" #'mw-thesaurus-lookup-dwim)
      (:prefix ("t" . "translate")
       "L" #'set-google-translate-languages
       "l" #'set-google-translate-target-language
       "Q" #'google-translate-query-translate-reverse
       "q" #'google-translate-query-translate
       "T" #'google-translate-at-point-reverse
       "t" #'google-translate-at-point))

(use-package! keytar
  :defer 5
  :after lsp-grammarly
  :config
  (require 'keytar))

(use-package! spacehammer
  :defer t
  :commands spacehammer/edit-with-emacs
  :config
  (add-hook! spacehammer/edit-with-emacs #'on-spacehammer-edit-with-emacs)
  (add-hook! spacehammer/before-finish-edit-with-emacs #'spacehammer-before-finish-edit-with-emacs))

(use-package! lsp-grammarly
  :defer t
  :commands spacehammer/edit-with-emacs lsp-grammarly-check-grammar
  :hook ((text-mode . lsp)
         (markdown-mode . lsp))
  :init
  (setq lsp-grammarly-auto-activate nil)
  :config
  (setq lsp-grammarly-domain "technical"
        lsp-grammarly-audience "expert")
  ;; TODO
  ;;(setq lsp-grammarly-active-modes (remove 'org-mode lsp-grammarly-active-modes))
  )

(use-package! mw-thesaurus
  :defer t
  :commands mw-thesaurus-lookup-dwim
  :hook (mw-thesaurus-mode . variable-pitch-mode)
  :config
  (map! :map mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
  (add-to-list
   'display-buffer-alist
   `(,mw-thesaurus-buffer-name
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.3))))

(use-package! sdcv-mode
  :defer t
  :commands sdcv-search sdcv-search-at-point
  :hook (sdcv-mode . visual-line-mode)
  :config
  (map! :map sdcv-mode-map
        :n "q" #'sdcv-return-from-sdcv
        :n "n" #'sdcv-next-entry
        :n "p" #'sdcv-previous-entry
        :ni "RET" #'sdcv-search-at-point
        :n "a" #'sdcv-search-at-point)
  (add-to-list
   'display-buffer-alist
   `(,sdcv-buffer-name
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.25))))

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
        google-translate-default-source-language "ru"
        google-translate-default-target-language "en")

  ;; to use 'listen' feature of google-translate, on Mac:
  ;; brew install mplayer-osx-extended
  ;; ln -s '/Applications/MPlayer OSX Extended.app/Contents/Resources/Binaries/mpextended.mpBinaries/Contents/MacOS/mplayer' /usr/local/bin/mplayer
  (setf google-translate-listen-program
        (if (eq system-type 'darwin)
            "/usr/local/bin/mplayer"
          "/usr/bin/mplayer"))
  (setq google-translate-input-method-auto-toggling t
        google-translate-preferable-input-methods-alist
        '((nil . ("en"))
          (russian-computer . ("ru"))))

  ;; it doesn't pop to the buffer automatically
  (defun google-translate--find-buffer (x)
    (pop-to-buffer "*Google Translate*"))

  (advice-add 'google-translate-buffer-output-translation :after #'google-translate--find-buffer)

  (add-to-list
   'display-buffer-alist
   '("\\*Google Translate\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.25))))

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
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.25))))

(use-package! flyspell
  :defer t
  :init
  (add-hook! (TeX-mode
              org-mode
              markdown-mode
              message-mode
              git-commit-mode) #'flyspell-mode)
  :config

  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package! flyspell-correct
  :defer t)

(use-package! flyspell-lazy
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(after! ispell
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (setq ispell-program-name "enchant-2")
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

  (defadvice! change-dict-after-toggle-input (a b)
    :after #'toggle-input-method
    (ispell-change-dictionary
     (if (string= current-input-method "russian-computer")
         "ru"
       nil))))