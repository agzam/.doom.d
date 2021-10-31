;;; custom/elisp/config.el -*- lexical-binding: t; -*-

(use-package! paradox
  :init
  (map! :leader
        (:prefix-map ("a" . "apps")
         :desc "List Packages" "k" #'paradox-list-packages))

  (map! :localleader
        :map paradox-menu-mode-map
        "r" #'paradox-filter-regexp
        "f" #'hydra-paradox-filter/body))
