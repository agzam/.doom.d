;;; custom/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode)

(use-package! jinja2-mode
  :mode "\\.jinja$"
  :config
  ;; The default behavior is to reindent the whole buffer on save. This is
  ;; disruptive and imposing. There are indentation commands available; the user
  ;; can decide when they want their code reindented.
  (setq jinja2-enable-indent-on-save nil))

(use-package! highlight-indent-guides
  :hook (yaml-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-suppress-auto-error t)
  :config
  (defun +indent-guides-init-faces-h (&rest _)
    (when (display-graphic-p)
      (highlight-indent-guides-auto-set-faces)))

  ;; HACK `highlight-indent-guides' calculates its faces from the current theme,
  ;;      but is unable to do so properly in terminal Emacs, where it only has
  ;;      access to 256 colors. So if the user uses a daemon we must wait for
  ;;      the first graphical frame to be available to do.
  (add-hook 'doom-load-theme-hook #'+indent-guides-init-faces-h)
  (when doom-theme
    (+indent-guides-init-faces-h))

  ;; `highlight-indent-guides' breaks when `org-indent-mode' is active
  (add-hook! 'org-mode-local-vars-hook
    (defun +indent-guides-disable-maybe-h ()
      (and highlight-indent-guides-mode
           (bound-and-true-p org-indent-mode)
           (highlight-indent-guides-mode -1)))))

(use-package! yaml-pro
  :after yaml-mode
  :hook (yaml-mode . yaml-pro-ts-mode)
  :config
  (map! :map yaml-pro-ts-mode-map
        [remap imenu] #'yaml-pro-jump
        "C-c C-f" nil
        :n "]]" #'yaml-pro-ts-next-subtree
        :n "[[" #'yaml-pro-ts-prev-subtree
        :n "[{" #'yaml-pro-ts-first-sibling
        :n "]}" #'yaml-pro-ts-last-sibling
        :n "M-l" #'yaml-pro-ts-indent-subtree
        :n "M-h" #'yaml-pro-ts-unindent-subtree
        :n "zc" #'yaml-pro-fold-at-point
        :n "zo" #'yaml-pro-unfold-at-point
        :n "gk" #'yaml-pro-ts-prev-subtree
        :n "gj" #'yaml-pro-ts-next-subtree
        :n "gK" #'yaml-pro-ts-up-level
        :n "gJ" #'yaml-pro-ts-down-level
        :n "M-k" #'yaml-pro-ts-move-subtree-up
        :n "M-j" #'yaml-pro-ts-move-subtree-down))
