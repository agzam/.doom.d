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
  (cl-defmethod eshell-output-object-to-target :around (_obj (target marker))
    ;; immediately open the redirected buffer
    (let ((base (cl-call-next-method)))
      (when (buffer-live-p (marker-buffer target))
        (with-current-buffer (marker-buffer target)
          (ansi-color-apply-on-region (point-min) (point-max))
          (display-buffer (current-buffer))))
      base))

 (add-hook! 'eshell-mode-hook
   (defun set-eshell-keys-h ()
     (map! :map eshell-mode-map
           :desc "clear" "C-c C-l" #'eshell-clear+
           :desc "history" "M-r" #'consult-history
           (:localleader
            :desc "clear" "c" #'eshell-clear+
            "b" #'eshell-insert-buffer-name))
     (map! :map eshell-hist-mode-map
           :desc "clear" "C-c C-l" #'eshell-clear+
           :desc "history" "M-r" #'consult-history))))

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

(after! eshell (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual)
  (dolist (cmd '(ncdu btop))
    (add-to-list 'eshell-visual-commands (symbol-name cmd))))
