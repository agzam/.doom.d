;;; custom/elisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'paradox-menu-mode--set-keys "custom/elisp/autoload" nil t)
(defun paradox-menu-mode--set-keys ()
  (map! :localleader
        :map paradox-menu-mode-map
        "r" #'paradox-filter-regexp
        "f" #'hydra-paradox-filter/body))
