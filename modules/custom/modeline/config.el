;;; custom/modeline/config.el -*- lexical-binding: t; -*-

(use-package! doom-modeline
  :init
  (require 'doom-modeline)
  (doom-modeline-def-modeline
   'agcustom
   '(bar persp-name workspace-name buffer-info)
   '(;; battery
     ;; grip
     ;; irc mu4e debug
     ;; repl
     lsp
     major-mode
     misc-info
     process
     ;; checker
     ;; buffer-position
     matches selection-info))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'agcustom))

  (add-hook 'doom-mode~ine-mode-hook 'setup-custom-doom-modeline 90)
  (add-hook 'window-state-change-hook 'setup-custom-doom-modeline)
  :config
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
        doom-modeline-bar-width 2)

  (doom-modeline-mode +1)
  ;; (defun doom-modeline--font-height () 5)
  )
