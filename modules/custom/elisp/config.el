;;; custom/elisp/config.el -*- lexical-binding: t; -*-

(use-package! paradox
  :init
  (map! :leader
        (:prefix-map ("a" . "apps")
         :desc "List Packages" "k" #'paradox-list-packages))
  :hook (paradox-menu-mode . #'paradox-menu-mode--set-keys))
