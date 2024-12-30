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
   lsp-headerline-breadcrumb-enable t
   lsp-completion-enable t
   lsp-enable-symbol-highlighting t
   lsp-enable-imenu nil
   lsp-treemacs-errors-position-params '((side . right))
   lsp-treemacs-sync-mode nil

   lsp-ui-sideline-enable nil
   lsp-ui-doc-enable nil
   lsp-ui-doc-position 'top
   lsp-modeline-code-actions-enable nil

   lsp-semantic-tokens-enable nil
   lsp-lens-enable t
   lsp-enable-indentation t)

  (map! :map lsp-mode-map
        [remap imenu] #'consult-lsp-file-symbols
        (:localleader
         "ge" #'lsp-ui-flycheck-list+
         (:prefix ("a" . "code actions")
                  "a" #'lsp-execute-code-action)
         (:prefix ("g" . "goto")
                  "n" #'lsp-find-declaration
                  "d" #'lsp-find-definition
                  "D" #'lsp-find-definition-other-window
                  "r" #'lsp-find-references)
         (:prefix ("f" . "format")
                  "b" #'lsp-format-buffer
                  "r" #'lsp-format-region
                  "i" #'lsp-organize-imports)
         (:prefix ("h" . "help")
                  "h" #'lsp-describe-thing-at-point)
         (:prefix ("t". "toggle")
                  "h" #'lsp--document-highlight
                  "L" #'lsp-lens-mode
                  "f" #'file-notify-rm-all-watches)
         (:prefix ("x" . "text/code")
                  "l" #'lsp-lens-show
                  "L" #'lsp-lens-hide)))

  (map! :map lsp-ui-flycheck-list-mode-map
        :nv "q" #'kill-buffer-and-window)

  (remove-hook! lsp-mode #'lsp-ui-mode #'+lookup--init-lsp-mode-handlers-h)

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(use-package! dap-mode
  :after lsp-mode
  :init
  (after! lsp-mode (require 'dap-mode))
  :config
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package! dap-ui
  :after dap-mode
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))
