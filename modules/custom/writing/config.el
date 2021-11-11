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
