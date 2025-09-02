;;; custom/webdev/config.el -*- lexical-binding: t; -*-

(add-hook! '(typescript-ts-mode-hook
            jtsx-jsx-mode-hook)
           #'+typescript-mode-lookup-handlers
           #'activate-ts-dash-docsets-h
           #'lsp!)

(add-hook! '(jtsx-tsx-mode-hook)
           #'emmet-mode)

(use-package! emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode reason-mode)
  :config
  (when (require 'yasnippet nil t)
    (add-hook 'emmet-mode-hook #'yas-minor-mode-on))
  (setq emmet-move-cursor-between-quotes t)
  (setq-hook! 'rjsx-mode-hook emmet-expand-jsx-className? t)
  (map! :map emmet-mode-keymap
        :v [tab] #'emmet-wrap-with-markup
        [tab] #'+web/indent-or-yas-or-emmet-expand
        "M-E" #'emmet-expand-line))

(use-package! prettier
  :defer t)

(use-package! html-to-hiccup
  :defer t)

(use-package! js-comint
  :defer t)

(use-package! lsp-eslint
  :defer t
  :after lsp-mode)

(use-package! jtsx
  :defer t
  :commands (jtsx-install-treesit-language)
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :config
  (setq jtsx-enable-jsx-element-tags-auto-sync t))
