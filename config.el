;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ag Ibragimov"
      user-mail-address "agzam.ibragimov@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq
 doom-font (font-spec :family "JetBrains Mono" :size 15)
 doom-variable-pitch-font (font-spec :family "Karla" :size 18)
 ;; doom-theme 'spacemacs-light
 )


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq-default
 line-spacing 6
 garbage-collection-messages nil
 left-fringe-width 6
 right-fringe-width 0
 evil-escape-key-sequence "kj"
 evil-esc-delay 0.3
 messages-buffer-max-lines 10000
 fill-column 90)

(setq
 doom-localleader-key ","
 doom-localleader-alt-key "C-,"
 chemacs-current-emacs-profile "doom"
 evil-want-C-u-scroll nil
 ;; auto-hscroll-mode 'current-line
 default-input-method 'russian-computer
 tab-width 4
 apropos-sort-by-scores t
 doom-font-increment 1
 split-width-threshold 160
 split-height-threshold 80
 switch-to-buffer-obey-display-actions t)

(after! epa
  (setq epg-pinentry-mode nil))

(after! dumb-jump
  ;; https://github.com/jacktasia/dumb-jump#emacs-options
  (setq dumb-jump-force-searcher 'rg))

(after! which-key
  (setq
   which-key-use-C-h-commands t
   which-key-show-early-on-C-h t
   which-key-idle-delay 0.5
   which-key-idle-secondary-delay 0.2)

  ;; replace 'evil-' in which-key HUD with a tiny triangle
  ;; borrowed from https://tecosaur.github.io/emacs-config/config.html
  (setq which-key-allow-multiple-replacements t)
  (after! which-key
    (pushnew!
     which-key-replacement-alist
     '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
     '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))

  (which-key-mode))

(setq markdown-enable-math nil)

(when (modulep! :custom general)
  (add-hook! 'window-setup-hook
    (defun position-frame-on-load-h ()
      ;; Emacs 29 changed font for the modeline
      ;; https://github.com/hlissner/doom-emacs/issues/5891#issuecomment-992758572
      (custom-set-faces! '(mode-line-active :inherit mode-line))

      (init-visual-line-keys)
      (global-visual-line-mode +1)
      (fringe-mode '(6 . 0))
      (toggle-frame-full-height)

      (when (fboundp 'pixel-scroll-precision-mode)
        (pixel-scroll-precision-mode +1))
      (run-with-timer
       1.2 nil
       #'place-frame-at-display-spot '(0 0 0.664 1.0)))))

(after! custom
  ;; in customize dialogs keep the elisp names
  (setq custom-unlispify-tag-names nil))

(add-hook! 'next-error-hook #'recenter)

(advice-remove 'evil-open-above #'+evil--insert-newline-above-and-respect-comments-a)
(advice-remove 'newline-and-indent #'+default--newline-indent-and-continue-comments-a)

(defalias 'elisp-mode 'emacs-lisp-mode)

;; disable global-hl-line
;; oddly, that's the way: https://github.com/hlissner/doom-emacs/issues/4206
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(after! evil
  (setq evil-jumps-cross-buffers t
        evil-move-cursor-back nil)
  (map! :map 'evil-visual-state-map "u" #'undo))

(after! better-jumper
  (setq better-jumper-context 'window))

(after! time
  (setq world-clock-list
        '(("America/Los_Angeles" "Pacific")
          ("America/Chicago" "Central")
          ("America/New_York" "Eastern")
          ("Europe/Paris" "Paris")
          ("Europe/Kiev" "Kiev")
          ("Europe/Moscow" "Moscow")
          ("Asia/Tashkent" "Tashkent"))))

(after! flycheck
  (ignore-error
   (define-key flycheck-mode-map flycheck-keymap-prefix nil))
  (setq flycheck-keymap-prefix nil)
  ;; (map! :leader "!" flycheck-command-map)
  )

(after! grep
  (setq grep-program "rg")
  ;; for whatever strange reason, embark-export started automatically enabling wgrep in
  ;; grep buffers. A workaround until I figure out what the heck.
  ;; (add-hook! 'embark-after-export-hook
  ;;   (defun embark-after-export-h ()
  ;;     (run-with-timer 0.1 nil (lambda () (quiet! (wgrep-abort-changes))))))
  )

(add-hook! 'prog-mode-hook #'hs-minor-mode)

;; disable visual-line-navigation in certain modes
(add-hook! (elfeed-search-mode
            gh-notify-mode grep-mode magit-log-mode
            notmuch-hello-mode notmuch-search-mode notmuch-tree-mode
            paradox-menu-mode yaml-mode)
  (defun no-wrap-h ()
    (+toggle-visual-line-navigation -1)))

(after! writeroom-mode
  (setq writeroom-maximize-window t))

(after! general
  (general-auto-unbind-keys))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

(define-key! :keymaps +default-minibuffer-maps
  "C-u" #'universal-argument)

;; needed additional binding, because can't emit backslash from Hammerspoon
(map! "C-<f12>" #'toggle-input-method)

(map! :map grep-mode-map
      :n "q" #'kill-buffer-and-window
      :n "[" #'compilation-previous-file
      :n "]" #'compilation-next-file
      (:localleader
       "f" #'next-error-follow-minor-mode))

;; disable nonsensical keys
(dolist (key '("s-n" "s-p" "s-q" "s-m" "C-x C-c"))
  (global-set-key (kbd key) nil))

;;; Globals
(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp
      :v "s" #'evil-surround-region
      "s-b" #'consult-buffer
      "s-=" #'text-scale-increase
      "s--" #'text-scale-decrease
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter))
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter))
      :n "zk" #'text-scale-increase
      :n "zj" #'text-scale-decrease
      :n "s-e" #'+scroll-line-down-other-window
      :n "s-y" #'+scroll-line-up-other-window)

(map! (:map minibuffer-mode-map
            "M-l" #'sp-forward-slurp-sexp
            "M-h" #'sp-forward-barf-sexp)
      (:map minibuffer-local-map
            "C-c C-s" #'embark-collect))

(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command
      "TAB"   #'alternate-buffer
      "v"     #'er/expand-region
      :desc "(un)comment" ";" #'evilnc-comment-or-uncomment-lines

      (:when (modulep! :custom shell)
        :desc "pop shell" "'" #'shell-pop
        :desc "choose shell" "\"" #'shell-pop-choose)

      (:prefix ("b" . "buffers")
       :desc "scratch" "s" #'doom/switch-to-scratch-buffer
       :desc "Messages" "m" #'switch-to-messages-buffer
       :desc "kill" "d" #'kill-this-buffer
       :desc "kill with window" "k" #'kill-buffer-and-window
       :desc "diff with file" "D" #'diff-current-buffer-with-file
       :desc "kill some buffers" "s-d" #'spacemacs/kill-matching-buffers-rudely)

      (:prefix ("e" . "edit")
       :desc "edit indirect" "i" #'edit-indirect-region)

      (:prefix ("f" . "files")
               (:when (modulep! :custom search)
                 :desc "fasd dir" "ad" (cmd! (+fasd-find 'dirs))
                 :desc "fasd file" "af" (cmd! (+fasd-find 'files))
                 :desc "fasd any" "aa" (cmd! (+fasd-find 'both)))
               :desc "dired" "j" #'dired-jump
               (:when IS-MAC
                 :desc "open in app" "O" #'+macos/open-in-default-program)
               "e" nil
               (:prefix ("e" . "doom/emacs")
                :desc "doom.d" "d" #'find-in-doom-dir
                :desc "doom init dir" "i" (cmd! (dired doom-emacs-dir))))

      (:prefix ("g" . "goto/git")
       :desc "magit file" "f" #'magit-file-dispatch
       :desc "jump list" "j" #'evil-show-jumps
       :desc "git status" "s" #'magit-status
       :desc "blame" "b" (cmd!
                          (call-interactively #'magit-blame-addition)
                          (magit-blame-cycle-style))
       (:prefix ("l" . "git link")
        :desc "blame link" "b" #'git-link-blame
        :desc "copy link" "l" #'git-link-kill
        :desc "main branch" "m" #'git-link-main-branch))

      (:prefix ("h" . "help")
               "a" #'helpful-at-point
               "f" #'helpful-function
               "h" #'helpful-symbol
               "p" nil
               (:prefix ("p" . "packages")
                        "l" #'list-packages
                        "f" #'find-library-other-window
                        "d" #'doom/describe-package)
               "s" #'find-function-other-window
               "v" #'helpful-variable
               "j" #'info-display-manual)

      (:prefix ("j" . "jump")
       "j" #'avy-goto-char-timer
       :desc "xwidget" "x" #'xwidget-webkit-url-get-create)

      (:prefix ("k" .  "lispy")
               "=" #'sp-reindent
               "-" #'sp-reindent
               "W" #'sp-unwrap-sexp
               "b" #'sp-forward-barf-sexp
               "B" #'sp-backward-barf-sexp
               "c" #'sp-convolute-sexp
               (:prefix ("d" . "kill")
                        "x" #'sp-kill-sexp)
               "r" #'sp-raise-sexp
               "s" #'sp-forward-slurp-sexp
               "S" #'sp-backward-slurp-sexp
               "t" #'sp-transpose-sexp
               "w" #'sp-wrap-sexp
               "y" #'sp-copy-sexp)

      (:when (modulep! :custom tab-bar)
        :desc "tab-bar" "l" #'tab-bar-transient)

      (:prefix ("n" . "narrow")
       "F" #'narrow-to-defun-indirect-buffer
       "R" #'narrow-to-region-indirect-buffer
       "f" #'narrow-to-defun
       "r" #'narrow-to-region
       "l" #'consult-focus-lines
       :desc "widen" "w" (cmd! (consult-focus-lines :show) (widen)))

      (:prefix ("o" . "open/Org")
       :desc "store link" "l" #'org-store-link
       :desc "link without id" "L" #'org-store-link-id-optional
       (:when (modulep! :custom notmuch)
         :desc "notmuch" "m" #'notmuch)
       (:when (modulep! :custom web-browsing)
         :desc "browser history" "b" #'browser-hist-search
         :desc "elfeed" "e" #'elfeed
         (:prefix ("h" . "HackerNews")
          :desc "top" "t" #'hnreader-best
          :desc "news" "n" #'hnreader-news))
       (:when (modulep! :custom git)
         (:prefix ("g" . "git")
                  "h" #'gh-notify)))

      (:prefix ("p" . "projects")
               (:after projectile
                :desc "Invalidate project cache" "I" #'projectile-invalidate-cache
                :desc "project IBuffer" "i" #'projectile-ibuffer
                :desc "find dir" "d" #'projectile-find-dir)
               (:when (modulep! :custom dired)
                 :desc "treemacs" "T" #'treemacs-project-toggle+
                 :desc "dired locate" "t" #'+dired-jump-find-in-project))

      (:prefix ("r" . "reset/resume/ring")
       :desc "yank from kill-ring" "y" #'consult-yank-from-kill-ring
       (:after vertico
        :desc "vertico repeat" "l" #'vertico-repeat-last)
       (:after corfu
        :desc "corfu reset" "c" #'+corfu-kill-frames))

      (:prefix ("s" . "search/symbol")
       :desc "google search" "/" #'engine/search-google
       :desc "eww search" "e" #'eww-search-words
       :desc "find-name-dired" "f" #'find-name-dired
       :desc "GitHub" "g" #'engine/search-github-with-lang
       :desc "imenu" "j" #'imenu)

      (:prefix ("t" . "toggle yo")
       :desc "v-line nav" "w" #'+toggle-visual-line-navigation
       :desc "minor modes" "m" #'consult-minor-mode-menu)

      (:prefix ("T" . "toggle global")
               (:when (modulep! :custom colors)
                 :desc "next color theme" "n" #'colors/cycle-themes-down
                 :desc "prev color theme" "p" #'colors/cycle-themes-up))

      (:prefix ("w" . "windows")
               "." #'window-transient
               "c" #'window-cleanup+
               "g" #'golden-ratio
               "D" #'ace-delete-window
               "M" #'ace-swap-window
               "W" #'ace-window
               "_" #'delete-other-windows-horizontally
               "m" #'toggle-maximize-buffer
               "|" #'delete-other-windows-vertically
               "r" #'balance-windows-area
               "=" #'balance-windows-area)
      "x" nil
      (:prefix ("x" ."text")
               "b" #'flyspell-correct-previous
               "x" #'flyspell-correct-at-point
               (:when (modulep! :custom writing)
                 (:prefix ("l" . "language")
                  :desc "define" "d" #'define-it-at-point
                  :desc "grammarly check" "g" #'lsp-grammarly-check-grammar
                  :desc "sdcv" "l" #'sdcv-search-pointer
                  :desc "Merriam Webster" "m" #'mw-thesaurus-lookup-dwim
                  :desc "wiktionary" "w" #'wiktionary-bro-dwim)
                 (:prefix ("g" . "translate")
                  :desc "en->ru" "e" #'google-translate-query-translate-reverse
                  :desc "ru->en" "r" #'google-translate-query-translate
                  :desc "es->en" "s" #'+google-translate-es->en
                  :desc "en->es" "S" #'+google-translate-en->es
                  :desc "translate" "g" #'google-translate-at-point)))

      (:prefix ("z" . "zoom")
       :desc "frame" "f" #'frame-zoom-transient))

(map! :map special-mode-map
      "SPC" nil
      "h" #'evil-backward-char)

(after! evil-maps
  ;; often conflicts with doom-local-leader
  ;; (unbind-key (kbd ",") evil-motion-state-map)
  (map! (:map evil-motion-state-map "C-u" nil)
        (:map evil-insert-state-map "C-u" nil)))

(map! :after vertico
      :map vertico-map
      "C-c C-p"  #'vertico-posframe-briefly-off
      "C-'" #'vertico-quick-insert
      "C-h" #'vertico-directory-delete-word
      "C-c C-g" #'vertico-grid-mode
      "M-h" #'vertico-grid-left
      "M-l" #'vertico-grid-right
      "M-j" #'vertico-next
      "M-k" #'vertico-previous
      "C-e" #'vertico-scroll-up
      "C-y" #'vertico-scroll-down
      "]" #'vertico-next-group
      "[" #'vertico-previous-group
      "~" #'vertico-jump-to-home-dir-on~)

(map! :after embark
      (:map embark-file-map
            "o" nil
            (:prefix ("o" . "open")
                     "j" (embark-split-action find-file +evil/window-split-and-follow)
                     "l" (embark-split-action find-file +evil/window-vsplit-and-follow)
                     "h" (embark-split-action find-file split-window-horizontally)
                     "k" (embark-split-action find-file split-window-vertically)
                     "a" (embark-ace-action find-file)))
      (:map embark-buffer-map
            "o" nil
            (:prefix ("o" . "open")
                     "j" (embark-split-action switch-to-buffer +evil/window-split-and-follow)
                     "a" (embark-ace-action switch-to-buffer)))
      (:map embark-function-map
            "o" nil
            (:prefix ("d" . "definition")
                     "j" (embark-split-action xref-find-definitions +evil/window-split-and-follow)
                     "l" (embark-split-action xref-find-definitions +evil/window-vsplit-and-follow)
                     "h" (embark-split-action xref-find-definitions split-window-horizontally)
                     "k" (embark-split-action xref-find-definitions split-window-vertically)
                     "a" (embark-ace-action xref-find-definitions)))
      (:map embark-url-map
            "e" #'+eww-open-in-other-window
            "b" #'+browse-url)
      (:map embark-collect-mode-map
       :n "[" #'embark-previous-symbol
       :n "]" #'embark-next-symbol)
      (:map (embark-command-map embark-symbol-map)
            (:after edebug
                    (:prefix ("D" . "debug")
                             "f" #'+edebug-instrument-symbol
                             "F" #'edebug-remove-instrumentation))))

(map! :after ibuffer
      :map ibuffer-mode-map
      :n "su" #'ibuffer-filter-by-unsaved-file-buffers
      :n "sF" #'ibuffer-filter-by-file-buffers)

(map! :map occur-mode-map
      :n "f" #'occur-mode-display-occurrence)

(map! :after consult
      :map isearch-mode-map "M-s l" #'consult-line)

(map! :after transient
      (:map transient-map
            "q" #'transient-quit-one
            "<escape>" #'transient-quit-one)
      (:map transient-edit-map "q" #'transient-quit-one)
      (:map transient-sticky-map "q" #'transient-quit-seq))

(map! :after helpful
      :map helpful-mode-map
      :n "q" #'kill-buffer-and-window)
