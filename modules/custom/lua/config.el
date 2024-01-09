;;; custom/lua/config.el -*- lexical-binding: t; -*-

(use-package! fennel-mode
  :mode "\\.fnl$"
  :defer t
  :hook (fennel-mode . lsp-mode)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(fennel-mode . "fennel"))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "fennel-ls")
      :activation-fn (lsp-activate-on "fennel")
      :server-id 'fennel-ls)))

  (set-lookup-handlers! 'fennel-mode
    ;; :documentation #'+consult-dash-doc
    :documentation #'fennel-show-documentation
    :definition #'fennel-find-definition)

  (when (eq system-type 'darwin)
    (add-hook! fennel-mode
      (defun fennel-mode-h ()
        (dash-docs-activate-docset "Hammerspoon")))))


(use-package! lua-mode
  :defer t
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level 2)
  :config
  (set-lookup-handlers! 'lua-mode :documentation 'lua-search-documentation))
