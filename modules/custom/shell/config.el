;;; custom/shell/config.el -*- lexical-binding: t; -*-

(map! :leader
        "'" #'shell-pop
        "\"" #'shell-pop-choose
        "p '" #'shell-pop-in-project-root)

(defun +insert-current-filename ()
  (interactive)
  (insert
   (buffer-file-name
    (window-buffer (minibuffer-selected-window)))))

(map! :map minibuffer-local-map "C-c C-i" #'+insert-current-filename)

(after! shell
  ;; Something messes up blue color in terminal. Usual way of setting it up in the theme,
  ;; not working for some reason. This is a hacky workaround
  (add-hook! '(shell-mode-hook doom-load-theme-hook)
    (defun set-shell-colors ()
      ;; I don't know how to find out if the current theme is dark or light
      (when (custom-theme-enabled-p 'ag-themes-base16-ocean)
        (set-face-attribute 'ansi-color-blue nil :foreground "#00bfff"))))

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

(use-package! vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")
