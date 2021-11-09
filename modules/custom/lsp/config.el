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
   lsp-lens-enable nil)

  (map! :localleader
        :map lsp-mode-map
        "ge" #'lsp-ui-flycheck-list+)

  (map! :map lsp-ui-flycheck-list-mode-map
        :nv "q" #'kill-buffer-and-window)
  )
