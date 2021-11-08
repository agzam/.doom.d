;;; custom/elisp/config.el -*- lexical-binding: t; -*-

(use-package! paradox
  :hook (paradox-menu-mode . paradox-menu-mode--set-keys)
  :init
  (paradox-enable)
  (setq paradox-column-width-package 30)
  (map! :leader "hdpl" #'list-packages))

(after! elisp-mode
 (map! :localleader
       :map emacs-lisp-mode-map
       :prefix "e"
       "c" #'eval-current-form-sp))
