;;; custom/elisp/config.el -*- lexical-binding: t; -*-

(use-package! paradox
  :hook (paradox-menu-mode . paradox-menu-mode--set-keys))

(after! elisp-mode
 (map! :localleader
       :map emacs-lisp-mode-map
       :prefix "e"
       "c" #'eval-current-form-sp))
