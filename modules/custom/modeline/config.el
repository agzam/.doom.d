;;; custom/modeline/config.el -*- lexical-binding: t; -*-

(use-package! doom-modeline
  :unless (featurep! :ui modeline)
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  :config
  (require 'doom-modeline)
  (doom-modeline-def-modeline
   'agcustom
   '(bar persp-name window-number buffer-info)
   '(;; battery
     ;; grip
     ;; irc mu4e debug
     ;; repl
     major-mode
     lsp
     misc-info
     process
     ;; checker

     matches selection-info
     buffer-position))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'agcustom))

  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline 90)
  (add-hook 'window-state-change-hook 'setup-custom-doom-modeline)

  (doom-modeline-mode +1)
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-modal-icon nil
        doom-modeline-mu4e nil
        doom-modeline-persp-icon t
        doom-modeline-display-default-persp-name t
        inhibit-compacting-font-caches t
        doom-modeline-height 15
        doom-modeline-bar-width left-fringe-width))
