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
  (add-hook! 'eshell-mode-hook
    (defun set-eshell-keys-h ()
      (map! :map eshell-mode-map
            :desc "clear" "C-c C-l" #'eshell-clear+
            :desc "detach" "C-<return>" #'eshell-send-detached-input+
            (:localleader
             :desc "clear" "c" #'eshell-clear+
             "b" #'eshell-insert-buffer-name))
      (map! :map eshell-hist-mode-map
            :desc "clear" "C-c C-l" #'eshell-clear+
            :desc "history" "M-r" #'consult-history
            :desc "output>buf" "C-c C-h" #'eshell-export-output+
            (:unless (featurep 'eshell-atuin)
              :desc "history" "M-r" #'consult-history)
            (:when (featurep 'eshell-atuin)
              :desc "history" "M-r" #'eshell-atuin-history))))

  ;; fullscreen apps
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual)
  (dolist (cmd '(ncdu btop k9s))
    (add-to-list 'eshell-visual-commands (symbol-name cmd))))

(use-package! shell-pop
  :defer t
  :init
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (interactive) (eshell))))
  :config
  (setq shell-pop-window-position "bottom"))


(use-package! vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'")

(use-package! eat
  :hook ((eshell-load . eat-eshell-mode)))

(use-package! eshell-atuin
  :when (executable-find "atuin")
  :after eshell
  :init (eshell-atuin-mode)
  :config
  (setopt
   eshell-atuin-search-fields '(time duration command directory relativetime)
   eshell-atuin-history-format "%-70c %>10r %-40i "
   eshell-atuin-filter-mode 'global
   eshell-atuin-search-options nil)

  (defadvice! eshell-atuin-history-fix-sorting-a (ofn &optional arg)
    :around #'eshell-atuin-history
    (let* ((vertico-sort-function nil))
      (funcall ofn arg))))
