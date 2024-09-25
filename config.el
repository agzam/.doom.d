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

(setq doom-theme 'ag-themes-spacemacs-light)

(unless (display-graphic-p)
  (setq doom-theme nil)
  ;; forcing thin cursor in insert mode
  (add-hook 'evil-insert-state-entry-hook
            (lambda () (send-string-to-terminal "\033[5 q")))
  (add-hook 'evil-normal-state-entry-hook
            (lambda () (send-string-to-terminal "\033[0 q")))
  (doom-disable-show-paren-mode-h))


(setq
 doom-font (font-spec :family "Fira Code" :size 16)
 doom-serif-font (font-spec :family "Fira Code" :size 16)
 doom-variable-pitch-font (font-spec :family "Noto Sans" :size 18)
 ;; doom-unicode-font (font-spec :family "Apple Color Emoji" :size 18)
 )

(add-hook! 'doom-load-theme-hook
  (defun reset-fixed-pitch-height-h ()
    "I don't know what exactly Doom is doing, but it's setting
     fixed-pitch number to absolute unit, should be relative"
    (set-face-attribute 'fixed-pitch nil :height 1.0)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)

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
 ;; line-spacing 0.3
 garbage-collection-messages nil
 left-fringe-width 6
 right-fringe-width 0
 evil-escape-key-sequence "kj"
 evil-esc-delay 0.3
 messages-buffer-max-lines 10000
 fill-column 70)

(setq
 default-text-properties '(line-height 1.4)
 doom-localleader-key ","
 doom-localleader-alt-key "C-,"
 chemacs-current-emacs-profile "doom"
 scroll-margin 1
 evil-want-C-u-scroll nil
 ;; auto-hscroll-mode 'current-line
 default-input-method 'russian-computer
 tab-width 4
 apropos-sort-by-scores t
 doom-font-increment 1
 split-width-threshold 160
 split-height-threshold 80
 switch-to-buffer-obey-display-actions t
 mouse-autoselect-window t
 other-window-scroll-default #'get-lru-window

 ;; per https://github.com/emacs-lsp/lsp-mode#performance
 read-process-output-max (* 10 1024 1024) ;; 10mb
 gc-cons-threshold 200000000)


;; (add-to-list 'evil-collection-key-blacklist ",")
;; (add-to-list 'evil-collection-key-blacklist "C-,")

(after! epa
  (setq epg-pinentry-mode nil))

(after! man
  ;; open man pages in the same window
  (setq Man-notify-method 'pushy))

(after! dumb-jump
  ;; https://github.com/jacktasia/dumb-jump#emacs-options
  (setq dumb-jump-force-searcher 'rg))

(after! which-key
  (setq
   which-key-use-C-h-commands t
   which-key-show-early-on-C-h t
   which-key-idle-delay 0.5
   which-key-idle-secondary-delay 0.2
   which-key-show-prefix 'echo)

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
      (fringe-mode '(6 . 0))

      (when (fboundp 'pixel-scroll-precision-mode)
        (pixel-scroll-precision-mode +1))
      ;; (run-with-timer
      ;;  1.5 nil
      ;;  (lambda ()
      ;;    (toggle-frame-full-height)
      ;;    (place-frame-at-display-spot '(0 0 0.664 1.0))))
      )))

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
        evil-move-cursor-back nil
        evil-in-single-undo t
        evil-want-fine-undo 'yes)
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
  (ignore-errors
    (define-key flycheck-mode-map flycheck-keymap-prefix nil))
  (setq flycheck-keymap-prefix nil)
  ;; (map! :leader "!" flycheck-command-map)

  (global-flycheck-mode -1)  ; I don't know why Doom enables is by default

  ;; (add-hook! flycheck-mode
  ;;   (defun disable-flycheck-popup-buffer ()
  ;;     (setq flycheck-display-errors-function #'ignore)))
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-package)
  )

(after! grep
  (setq grep-program "rg")
  ;; for whatever strange reason, embark-export started automatically enabling wgrep in
  ;; grep buffers. A workaround until I figure out what the heck.
  ;; (add-hook! 'embark-after-export-hook
  ;;   (defun embark-after-export-h ()
  ;;     (run-with-timer 0.1 nil (lambda () (quiet! (wgrep-abort-changes))))))
  )

(after! grip-mode
  (setq grip-preview-use-webkit (featurep :system 'macos))
  (setq grip-github-user "agzam")
  (setf grip-github-password (auth-host->pass "api.github.com")))

(add-hook! 'prog-mode-hook
           #'hs-minor-mode
           #'visual-line-mode)

;; disable visual-line-navigation in certain modes
;; (add-hook! (elfeed-search-mode
;;             gh-notify-mode grep-mode magit-log-mode magit-refs-mode
;;             notmuch-hello-mode notmuch-search-mode notmuch-tree-mode
;;             paradox-menu-mode yaml-mode
;;             vc-annotate-mode)
;;   (defun no-wrap-h ()
;;     (+toggle-visual-line-navigation -1)))

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
(dolist (key '("s-n" "s-p" "s-q" "s-m" "s-,"
               "C-x C-c"
               "C-<tab>" "C-S-<tab>" "<f11>"
               "M-k" "M-j"))
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
      :n "s-r" #'+scroll-line-up-other-window
      :n "s-y" #'+scroll-line-up-other-window
      :n "s-u" #'+scroll-line-down-other-window
      :i "M-/" #'hippie-expand
      :n "gi" #'ibuffer-sidebar-jump
      :i "C-v" #'evil-paste-after
      :i "TAB" #'completion-at-point
      "C-x m" #'mpv-transient
      (:when (featurep :system 'linux)
        :i "C-M-S-s-y" #'nerd-dictation-toggle))

(map! (:map minibuffer-mode-map
            "M-l" #'sp-forward-slurp-sexp
            "M-h" #'sp-forward-barf-sexp)
      (:map minibuffer-local-map
            "C-c C-s" #'embark-collect))

(map! :after rfc-mode
      :map rfc-mode-map
      :n "q" #'quit-window
      :n "[[" #'rfc-mode-previous-section
      :n "]]" #'rfc-mode-next-section
      :n "C-K" #'rfc-mode-previous-section
      :n "C-j" #'rfc-mode-next-section)

;;;;;;;;;;;;;;;;;;;;;;;
;; Leader keybidings ;;
;;;;;;;;;;;;;;;;;;;;;;;

(map! :leader
      :desc "M-x" "SPC" #'execute-extended-command
      "TAB"   #'alternate-buffer
      "v"     #'expreg-transient
      :nv   ";" (cmd! (call-interactively
                       (if (evil-visual-state-p)
                           #'comment-or-uncomment-region
                         #'comment-line)))
      (:when (modulep! :custom shell)
        :desc "pop shell" "'" #'shell-pop
        :desc "choose shell" "\"" #'shell-pop-choose)

      (:prefix ("b" . "buffers/browser")
       :desc "scratch" "s" #'doom/switch-to-scratch-buffer
       :desc "Messages" "m" #'switch-to-messages-buffer
       :desc "kill" "d" #'kill-current-buffer
       :desc "kill with window" "k" #'kill-buffer-and-window
       :desc "diff with file" "D" #'diff-current-buffer-with-file
       :desc "kill some buffers" "s-d" #'spacemacs/kill-matching-buffers-rudely
       (:when (modulep! :custom web-browsing)
         :desc "browser history" "h" #'browser-hist-search
         :desc "browser tabs" "t" #'browser-goto-tab
         :desc "browser copy link" "l" #'browser-copy-tab-link
         :desc "insert url" "y" #'browser-insert-link-from-active-tab))

      (:prefix ("e" . "edit")
       :desc "edit indirect" "i" #'edit-indirect-region)

      (:prefix ("f" . "files")
               (:when (modulep! :custom search)
                 :desc "zoxide dir" "d" (cmd! (+zoxide-find)))
               :desc "dired" "j" #'dired-jump
               (:when (featurep :system 'macos)
                 :desc "open in app" "O" #'+macos/open-in-default-program)
               "e" nil
               (:prefix ("e" . "doom/emacs")
                :desc "doom.d" "d" #'find-in-doom-dir
                :desc "doom init dir" "i" (cmd! (dired doom-emacs-dir))
                (:when (featurep :system 'linux)
                  :desc "awesomewm config" "a" (cmd! (dired "~/.config/awesome/")))))

      (:prefix ("g" . "goto/git")
       :desc "magit file" "f" #'magit-file-dispatch
       :desc "jump list" "j" #'evil-show-jumps
       :desc "git status" "s" #'magit-status
       :desc "blame" "b" #'magit-blame-addition
       (:prefix ("c" . "consult-gh")
                "o" #'consult-gh-orgs
                "r" #'consult-gh-search-repos
                "f" #'consult-gh-find-file
                "i" #'consult-gh-issue-list
                "p" #'consult-gh-pr-list)
       (:prefix ("l" . "git link")
        :desc "blame link" "b" #'git-link-blame
        :desc "copy link" "l" #'git-link-kill
        :desc "main branch" "m" #'git-link-main-branch))

      (:prefix ("h" . "help")
               "a" #'helpful-at-point
               "f" #'helpful-function
               "h" #'helpful-symbol
               "c" #'consult-info
               "C" #'describe-key-briefly
               "p" nil
               (:prefix ("p" . "packages")
                        "l" #'list-packages
                        "f" #'find-library-other-window
                        "d" #'doom/describe-package)
               "s" #'find-function-other-window
               "v" #'helpful-variable
               "j" #'info-display-manual)

      (:prefix ("i" . "insert")
       :desc "snippet" "s" #'consult-yasnippet)

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
       :desc "widen" "w" (cmd! () (consult-focus-lines nil :show) (widen)))

      (:prefix ("o" . "open/Org")
       :desc "store link" "l" #'org-store-link
       :desc "link without id" "L" #'org-store-link-id-optional
       (:when (modulep! :custom notmuch)
         :desc "notmuch" "m" #'notmuch)
       (:when (modulep! :custom web-browsing)
         :desc "elfeed" "e" #'elfeed)
       (:when (modulep! :custom git)
         (:prefix ("g" . "git")
                  "h" #'gh-notify))
       (:prefix ("c" . "chat")
                "t" #'telega
                (:when (modulep! :custom ai)
                  :desc "ChatGPT" "g" #'gptel+))
       "r" nil
       (:prefix ("r" . "roam")
        "r" #'org-roam-node-find
        :desc "work today" "t" (cmd! (funcall-interactively #'org-roam-dailies-goto-today '("w")))
        :desc "journal today" "T" (cmd! (funcall-interactively #'org-roam-dailies-goto-today '("j")))
        :desc "work note" "n" (cmd! (funcall-interactively #'org-roam-dailies-goto-date nil '("w")))
        :desc "org-roam-ui in xwidget" "w" #'org-roam-toggle-ui-xwidget
        :desc "org-roam-ui in browser" "W" #'org-roam-ui-browser+
        "b" #'browser-create-roam-node-for-active-tab))

      (:prefix ("p" . "projects")
               (:after projectile
                :desc "Invalidate project cache" "I" #'projectile-invalidate-cache
                :desc "project IBuffer" "i" #'projectile-ibuffer
                :desc "find dir" "d" #'projectile-find-dir)
               (:when (modulep! :custom dired)
                 :desc "treemacs" "T" #'treemacs-project-toggle+
                 :desc "dired locate" "t" #'+dired-jump-find-in-project)
               (:when (modulep! :custom shell)
                 :desc "project shell" "'" #'shell-pop-in-project-root))

      (:prefix ("r" . "reset/resume/ring/roam")
       "r" #'org-roam-node-find
       :desc "yank from kill-ring" "y" #'consult-yank-from-kill-ring
       (:after vertico
        :desc "vertico repeat" "l" #'vertico-repeat
        :desc "vertico history" "L" #'vertico-repeat-select)
       (:after corfu
        :desc "corfu reset" "c" #'+corfu-kill-frames))

      (:prefix ("s" . "search/symbol")
       :desc "search" "/" #'consult-omni-transient
       :desc "eww search" "e" #'eww-search-words
       :desc "find-name-dired" "f" #'find-name-dired
       :desc "GitHub" "g" #'+search-github-with-lang
       :desc "imenu" "j" #'imenu)

      (:prefix ("t" . "toggle yo")
       :desc "v-line nav" "w" #'+toggle-visual-line-navigation
       :desc "minor modes" "m" #'consult-minor-mode-menu
       :desc "iBuffer side" "i" #'ibuffer-sidebar-toggle-sidebar
       :desc "Dired side" "d" #'dired-sidebar-toggle-sidebar
       :desc "line numbers" "l" #'display-line-numbers-mode)

      (:prefix ("T" . "toggle global")
       :desc "numbers" "N" #'global-display-line-numbers-mode
       (:when (modulep! :custom colors)
         :desc "next color theme" "n" #'colors/cycle-themes-down
         :desc "prev color theme" "p" #'colors/cycle-themes-up))

      (:prefix ("w" . "windows")
               "TAB" #'evil-window-prev
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
               "x" #'jinx-correct-word
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
                  :desc "translate" "g" #'google-translate-at-point))
               (:when (modulep! :custom ai)
                 (:prefix ("c" . "chatgpt")
                  :desc "chatgpt" "c" #'gptel+
                  :desc "check text" "e" #'+gptel-improve-text-transient
                   "s" #'gptel-send)))

      (:prefix ("z" . "zoom")
       :desc "frame" "f" #'frame-zoom-transient))

(map! :map special-mode-map
      "SPC" nil
      "h" #'evil-backward-char)

(after! (:and evil evil-maps)
  ;; often conflicts with doom-local-leader
  ;; (unbind-key (kbd ",") evil-motion-state-map)
  (map! (:map evil-motion-state-map "C-u" nil)
        (:map evil-insert-state-map "C-u" nil)
        (:map evil-window-map
              "L" #'+evil-window-move-right
              "H" #'+evil-window-move-left)))

(map! :after ibuffer
      :map ibuffer-mode-map
      [remap imenu] #'ibuffer-jump-to-buffer
      :n "su" #'ibuffer-filter-by-unsaved-file-buffers
      :n "sF" #'ibuffer-filter-by-file-buffers
      :n "s*" #'ibuffer-filter-by-non-special-buffers)

(map! :map occur-mode-map
      :n "f" #'occur-mode-display-occurrence)

(map! :after transient
      (:map transient-map
            "q" #'transient-quit-one
            "<escape>" #'transient-quit-one)
      (:map transient-edit-map "q" #'transient-quit-one)
      (:map transient-sticky-map "q" #'transient-quit-seq))

(map! :after helpful
      :map helpful-mode-map
      :n "q" #'kill-buffer-and-window)

(map! :after calendar
      :map calendar-mode-map
      :n "gd" #'calendar-goto-date)

(after! epa
  (setq epg-pinentry-mode 'loopback))

(use-package! ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(after! undo-fu
  (setq undo-limit 80000000 ; 80Mb
        undo-strong-limit 120000000 ; 120Mb
        ;; 400Mb
        undo-outer-limit 400000000))
