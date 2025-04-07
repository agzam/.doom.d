;;; custom/modeline/config.el -*- lexical-binding: t; -*-

(use-package! doom-modeline
  :unless (modulep! :ui modeline)
  :hook ((doom-modeline-mode . column-number-mode)
         (doom-modeline-mode . setup-custom-doom-modeline)
         (window-state-change . setup-custom-doom-modeline)
         (doom-init-ui . doom-modeline-mode))
  :after-call doom-first-input-hook doom-first-file-hook
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  :config
  (setopt doom-modeline-buffer-encoding nil
          doom-modeline-buffer-file-name-style 'relative-to-project
          doom-modeline-buffer-modification-icon t
          doom-modeline-buffer-state-icon t
          doom-modeline-icon (display-graphic-p)
          doom-modeline-major-mode-color-icon nil
          doom-modeline-major-mode-icon nil
          doom-modeline-modal-icon t
          doom-modeline-mu4e nil
          mode-line-compact nil
          inhibit-compacting-font-caches t
          doom-modeline-height 1
          doom-modeline-bar-width 4)

  ;; keep modeline short
  (defadvice! doom-modeline--font-height-a ()
    :override #'doom-modeline--font-height
    1))
