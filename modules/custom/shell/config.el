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
  (setq eshell-aliases-file (concat doom-emacs-dir ".local/etc/eshell/aliases"))
  (add-hook! 'eshell-mode-hook
    (defun set-eshell-keys-h ()
      (map! :map eshell-mode-map
            :desc "clear" "C-c C-l" #'eshell-clear+
            :desc "detach" "C-<return>" #'eshell-send-detached-input
            :desc "kitty detach" "s-<return>" #'eshell-send-detached-input-to-kitty
            :i "C-u" nil
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

  (add-hook! '(comint-mode-hook
               eshell-mode-hook)
             #'update-hyprland-env-h)

  (cl-defmethod eshell-output-object-to-target :around (_obj (target marker))
    ;; immediately open the redirected buffer
    (let ((base (cl-call-next-method)))
      (when (buffer-live-p (marker-buffer target))
        (with-current-buffer (marker-buffer target)
          (ansi-color-apply-on-region (point-min) (point-max))
          (display-buffer (current-buffer))))
      base))

  ;; fullscreen apps
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual)
  (dolist (cmd '(ncdu btop htop k9s pueue procs lazydocker))
    (add-to-list 'eshell-visual-commands (symbol-name cmd))))

(use-package! shell-pop
  :defer t
  :init
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (interactive) (eshell))))
  :config
  (setq shell-pop-window-position "bottom"))


(use-package! vimrc-mode
  :defer t
  :mode "\\.vim\\(rc\\)?\\'")

(use-package! eat
  :defer t
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

(use-package! yuck-mode
  :defer t)

(use-package! mise
  :defer t
  ;; :hook (doom-init-ui . global-mise-mode)
  :when (executable-find "mise"))

(defun on-mxp-buffer-update-h (buffer-name beg end)
  (with-current-buffer buffer-name
    (ansi-color-apply-on-region beg end)))

(add-hook 'mxp-buffer-update-hook #'on-mxp-buffer-update-h)
