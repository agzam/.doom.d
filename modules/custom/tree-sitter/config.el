;;; custom/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package! treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (tree-sitter-init+))
