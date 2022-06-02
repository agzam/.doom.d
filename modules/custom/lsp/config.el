;;; custom/lsp/config.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (setq
   ;; lsp-after-open-hook nil
   lsp-eldoc-enable-hover t
   lsp-eldoc-render-all nil
   lsp-modeline-diagnostics-enable nil
   ;; lsp-diagnostics-provider :flycheck
   ;; lsp-enable-file-watchers nil
   lsp-modeline--enable-code-actions nil
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-completion-at-point t
   lsp-enable-symbol-highlighting t
   lsp-enable-imenu nil
   lsp-treemacs-errors-position-params '((side . right))
   lsp-treemacs-sync-mode nil

   lsp-ui-sideline-enable nil
   lsp-ui-doc-enable nil
   lsp-ui-doc-position 'top
   lsp-completion-enable nil
   lsp-modeline-code-actions-enable nil

   lsp-semantic-tokens-enable nil
   lsp-lens-enable nil
   lsp-enable-indentation t)

  (map! :localleader
        :map lsp-mode-map
        "ge" #'lsp-ui-flycheck-list+
        (:prefix ("a" . "code actions")
         "a" #'lsp-execute-code-action)
        (:prefix ("g" . "goto")
         "d" #'xref-find-definitions
         "D" #'xref-find-definitions-other-window
         "r" #'xref-find-references)
        (:prefix ("f" . "format")
         "b" #'lsp-format-buffer
         "r" #'lsp-format-region
         "i" #'lsp-organize-imports)
        (:prefix ("h" . "help")
         "h" #'lsp-describe-thing-at-point)
        (:prefix ("t". "toggle")
         "h" #'lsp--document-highlight
         "L" #'lsp-lens-mode)
        (:prefix ("x" . "text/code")
         "l" #'lsp-lens-show
         "L" #'lsp-lens-hide))

  (map! :map lsp-ui-flycheck-list-mode-map
        :nv "q" #'kill-buffer-and-window)

  (remove-hook! 'lsp-mode-hook #'lsp-ui-mode))
