;;; custom/nix/config.el -*- lexical-binding: t; -*-

(use-package! nix-mode
  :defer t
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'"
  :init
  ;; Treat flake.lock files as json. Fall back to js-mode because it's faster
  ;; than js2-mode, and its extra features aren't needed there.
  (add-to-list 'auto-mode-alist
               (cons "/flake\\.lock\\'"
                     (if (modulep! :lang json)
                         'json-mode
                       'js-mode)))
  :config
  (set-repl-handler! 'nix-mode #'+nix/open-repl)
  (when (modulep! +lsp)
    (add-hook 'nix-mode-local-vars-hook #'lsp! 'append))

  (map! :localleader
        :map nix-mode-map
        "f" #'nix-update-fetch
        "p" #'nix-format-buffer
        "r" #'nix-repl-show
        "s" #'nix-shell
        "b" #'nix-build
        "u" #'nix-unpack))

(use-package! nix-update
  :defer t
  :commands nix-update-fetch)

(use-package! nix-repl
  :defer t
  :commands nix-repl-show)
