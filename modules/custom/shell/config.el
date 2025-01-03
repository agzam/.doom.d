;;; custom/shell/config.el -*- lexical-binding: t; -*-
(defun +insert-current-filename ()
  (interactive)
  (when (eq major-mode 'minibuffer-mode)
    (let ((fname (with-current-buffer
                     (window-buffer (minibuffer-selected-window))
                   (pcase major-mode
                     ('dired-mode
                      (dired-get-filename))
                     (_ buffer-file-name)))))
      (insert fname))))

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
        "C-j" nil
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

(use-package! system-packages
  :defer t
  :config
  (when (featurep :system 'linux)
    (add-to-list
     'system-packages-supported-package-managers
     '(yay .
       ((default-sudo . nil)
        (install . "yay -S")
        (search . "yay -Ss")
        (uninstall . "yay -Rns")
        (update . "yay -Syu")
        (clean-cache . "yay -Sc")
        (change-log . "yay -Qc")
        (log . "cat /var/log/pacman.log")
        (get-info . "yay -Qi")
        (get-info-remote . "yay -Si")
        (list-files-provided-by . "yay -qQl")
        (owning-file . "yay -Qo")
        (owning-file-remote . "yay -F")
        (verify-all-packages . "yay -Qkk")
        (verify-all-dependencies . "yay -Dk")
        (remove-orphaned . "yay -Rns $(yay -Qtdq)")
        (list-installed-packages . "yay -Qe")
        (list-installed-packages-all . "yay -Q")
        (list-dependencies-of . "yay -Qi")
        (noconfirm . "--noconfirm"))))
    (setq system-packages-package-manager 'yay)))

(use-package! eat
  :hook ((eshell-load . eat-eshell-mode)))
