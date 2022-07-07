;;; custom/modeline/config.el -*- lexical-binding: t; -*-

(use-package! doom-modeline
  :unless (featurep! :ui modeline)
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
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-modal-icon nil
        doom-modeline-mu4e nil
        mode-line-compact t
        inhibit-compacting-font-caches t
        doom-modeline-height 1)
  (setf doom-modeline-bar-width (or left-fringe-width 6))

  ;; keep modeline short
  (defadvice! doom-modeline--font-height-a ()
    :override #'doom-modeline--font-height
    1))
