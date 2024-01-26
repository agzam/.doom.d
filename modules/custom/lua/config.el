;;; custom/lua/config.el -*- lexical-binding: t; -*-

(use-package! fennel-mode
  :mode "\\.fnl$"
  :defer t
  :hook (fennel-mode . lsp)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(fennel-mode . "fennel"))

    (defun fennel-ls-init-options ()
      (let* ((lsp-cfg-dir (concat (projectile-project-root) "/.lsp/"))
             (cfg-file (expand-file-name "fennel-ls.json" lsp-cfg-dir))
             (json-object-type 'plist))
        (when (file-exists-p cfg-file)
          (json-read-file cfg-file))))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "fennel-ls")
      :activation-fn (lsp-activate-on "fennel")
      :server-id 'fennel-ls
      :initialization-options #'fennel-ls-init-options))

    (add-hook! lsp-mode
      (defun lsp-mode-bindings-override-h ()
        ;; fennel lsp-server doesn't yet support textDocument/documentSymbol
        (map! :map lsp-mode-map
              [remap imenu]
              (cmd! ()
                    (if (eq major-mode 'fennel-mode)
                        (call-interactively #'consult-imenu)
                      (call-interactively #'consult-lsp-file-symbols)))))))

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


(use-package! friar
  :defer t
  :init
  (defalias 'awesomewm-repl 'friar)
  :config
  (setq friar-fennel-file-path "fennel"))
