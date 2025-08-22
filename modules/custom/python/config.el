;;; custom/python/config.el -*- lexical-binding: t; -*-

(use-package! python
  :defer t
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :config
  (add-hook! (python-ts-mode python-mode) #'lsp!))

(use-package! lsp-pyright
  :defer t
  :init
  ;; important: this needs to be set before the package loads
  (setopt lsp-pyright-multi-root nil)
  :config
  (setopt
   ;; don't forget to `pip install basedpyright'
   lsp-pyright-langserver-command "basedpyright"
   lsp-pyright-venv-path "."
   lsp-pyright-venv-directory ".venv")

  (setopt lsp-pyright-type-checking-mode "off")

  (lsp-register-custom-settings
   `(("basedpyright.analysis.typeCheckingMode" lsp-pyright-type-checking-mode)))

  (lsp-dependency
   'pyright
   `(:system ,(concat (mise-python-dir) "/bin/basedpyright-langserver"))))
