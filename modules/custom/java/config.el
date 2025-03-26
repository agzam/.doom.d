;;; custom/java/config.el -*- lexical-binding: t; -*-

(use-package! lsp-java
  :defer t
  :preface
  (setopt lsp-java-workspace-dir (concat doom-data-dir "java-workspace"))
  (add-hook! '(java-mode-local-vars-hook java-ts-mode-hook)
             #'lsp! 'append))
