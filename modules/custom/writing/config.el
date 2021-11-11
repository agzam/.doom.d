;;; custom/writing/config.el -*- lexical-binding: t; -*-
(use-package! keytar
  :defer 5
  :config
  (require 'keytar))

(use-package! spacehammer
  :defer t
  :commands spacehammer/edit-with-emacs
  :config
  (add-hook! spacehammer/edit-with-emacs #'on-spacehammer-edit-with-emacs)
  (add-hook! spacehammer/before-finish-edit-with-emacs #'spacehammer-before-finish-edit-with-emacs))

(use-package! lsp-grammarly
  :after lsp text-mode markdown-mode
  :commands on-spacehammer-edit-with-emacs
  :hook ((text-mode . lsp)
         (markdown-mode . lsp))
  :init
  (setq lsp-grammarly-auto-activate nil)
  :config
  (setq lsp-grammarly-domain "technical"
        lsp-grammarly-audience "expert")
  (map! :leader "xlg" #'lsp-grammarly-check-grammar)

  ;; TODO
  ;;(setq lsp-grammarly-active-modes (remove 'org-mode lsp-grammarly-active-modes))
  )

(use-package! mw-thesaurus
  :defer t
  :config
  (map! :map mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
  (add-hook 'mw-thesaurus-mode-hook 'variable-pitch-mode)
  (map! :leader "xlm" #'mw-thesaurus-lookup-dwim)

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
  :config
  (add-hook 'sdcv-mode-hook #'visual-line-mode)

  (map! :leader "xll" #'sdcv-search-at-point)

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
  :functions (my-google-translate-at-point google-translate--search-tkk)
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  (map! :leader
        "xtL" #'set-google-translate-languages
        "xtl" #'set-google-translate-target-language
        "xtQ" #'google-translate-query-translate-reverse
        "xtq" #'google-translate-query-translate
        "xtT" #'google-translate-at-point-reverse
        "xtt" #'google-translate-at-point)

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
  :config
  (setq
   define-it-show-google-translate nil
   define-it-show-header nil)

  (map! :leader "xld" #'define-it-at-point)

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

(after! ispell
  (setq flyspell-issue-message-flag nil)    ; printing a message for every word has a negative performance impact
  (setq ispell-program-name "aspell")
  ;; aspell suggestion mode
  (add-to-list 'ispell-extra-args "--sug-mode=bad-spellers")
  (require 'flyspell)

  ;; Change dictionary with the input-method
  (defun change-dict-after-toggle-input (_ _)
    (ispell-change-dictionary
     (if (string= current-input-method "russian-computer")
         "ru"
       nil)))

  (advice-add 'toggle-input-method :after 'change-dict-after-toggle-input))
