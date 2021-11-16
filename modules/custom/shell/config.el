;;; custom/shell/config.el -*- lexical-binding: t; -*-

(map! :leader
        "'" #'shell-pop
        "\"" #'shell-pop-choose)

(after! shell
  (map! :map shell-mode-map
        "C-c C-l" #'comint-clear-buffer
        :localleader
        "c" #'comint-clear-buffer))

(after! eshell
  (map! :map eshell-mode-map
        :desc "clear" "C-c C-l" #'eshell-clear+
        :localleader
        :desc "clear" "c" #'eshell-clear+)
  (map! :map eshell-hist-mode-map
        :desc "clear" "C-c C-l" #'eshell-clear+))

(use-package! shell-pop
  :defer t
  :config
  (setq shell-pop-window-position "bottom"))
