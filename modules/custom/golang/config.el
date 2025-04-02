;;; custom/golang/config.el -*- lexical-binding: t; -*-

(use-package! go-mode
  :mode "\\.go\\'"
  :defer t
  :config
  (set-lookup-handlers! 'go-mode
    :documentation #'godoc-at-point)

  (add-hook! '(go-mode-local-vars-hook
               go-ts-mode-hook)
             #'tree-sitter!
             #'lsp 'append))

(use-package! gorepl-mode
  :commands gorepl-run-load-current-file
  :init
  (set-repl-handler! 'go-mode #'gorepl-run))

(use-package! gotest
  :defer t
  :after go-mode
  :commands (go-test-current-test go-test-current-file go-test-current-project)
  :config
  (map! :map (go-mode-map go-ts-mode-map)
        (:localleader
         (:prefix ("t" . "test")
                  "t" #'go-test-current-test
                  "n" #'go-test-current-file
                  "p" #'go-test-current-project))))
