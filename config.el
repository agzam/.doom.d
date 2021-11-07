;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ag Ibragimomv"
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
 doom-variable-pitch-font (font-spec :family "Open Sans" :size 15)
 ;; doom-theme 'spacemacs-light
 )

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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
 messages-buffer-max-lines 10000)

(setq
 doom-localleader-key ","
 which-key-use-C-h-commands t
 which-key-idle-delay 0.3
 which-key-idle-secondary-delay 0.2
 chemacs-current-emacs-profile "doom")

(map! :leader
      (:when (featurep! :completion vertico)
       :desc "M-x" :n "SPC" #'execute-extended-command))

(map! :leader :n "fj" #'dired-jump)

(map! :map vertico-map
      "C-e" #'vertico-scroll-up
      "C-y" #'vertico-scroll-down)

(map! :map company-active-map "C-/" #'completion-at-point)

;; vertico module screws up the backspace
(after! vertico
  (map! :map vertico-map [backspace] nil))

(map! :v "s" #'evil-surround-region)

(center-frame-horizontally)
(fringe-mode '(6 . 0))

;; disable global-hl-line
;; oddly that's the way: github.com/hlissner/doom-emacs/issues/4206
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
