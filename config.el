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
 doom-variable-pitch-font (font-spec :family "Open Sans" :size 18)
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
 split-height-threshold 80)

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

(after! company
  (setq company-show-numbers t
        company-tooltip-limit 10)
  (map! :map company-active-map "C-/" #'completion-at-point))

(setq markdown-enable-math nil)

;; most keys set in ':custom general' module,
;; yet the most important one I want to set early
(map! :leader
      (:when (featurep! :completion vertico)
       :desc "M-x" :n "SPC" #'execute-extended-command))

(when (featurep! :custom general)
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

(define-key! :keymaps +default-minibuffer-maps
  "C-u" #'universal-argument)

(defalias 'elisp-mode 'emacs-lisp-mode)

;; disable global-hl-line
;; oddly, that's the way: https://github.com/hlissner/doom-emacs/issues/4206
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; needed additional binding, because can't emit backslash from Hammerspoon
(map! "C-<f12>" #'toggle-input-method)

(after! evil
  (setq evil-jumps-cross-buffers t
        evil-move-cursor-back nil))

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
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix nil)
  (map! :leader "!" flycheck-command-map))

(after! helpful
  (map! :map helpful-mode-map
        :n "q" #'kill-buffer-and-window))

(after! grep
  (map! :map grep-mode-map
        :n "q" #'kill-buffer-and-window
        :n "[" #'compilation-previous-file
        :n "]" #'compilation-next-file
        (:localleader
         "f" #'next-error-follow-minor-mode))

  (add-hook! 'grep-mode-hook #'next-error-follow-minor-mode))
