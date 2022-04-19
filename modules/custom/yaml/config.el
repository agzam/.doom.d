;;; custom/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode)

(use-package! yaml-imenu
  :after yaml-mode)

(use-package! jinja2-mode
  :config
  ;; The default behavior is to reindent the whole buffer on save. This is
  ;; disruptive and imposing. There are indentation commands available; the user
  ;; can decide when they want their code reindented.
  (setq jinja2-enable-indent-on-save nil))
