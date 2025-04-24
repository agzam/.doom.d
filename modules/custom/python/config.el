;;; custom/python/config.el -*- lexical-binding: t; -*-

(use-package! python
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
   lsp-pyright-langserver-command "basedpyright"
   lsp-pyright-venv-path "."
   lsp-pyright-venv-directory ".venv")

  (defadvice! lsp-pyright-locate-python-use-mise (_)
    :around #'lsp-pyright-locate-python
    (concat (mise-python-dir) "/bin/python"))
  (lsp-dependency
   'pyright
   `(:system ,(concat (mise-python-dir) "/bin/basedpyright-langserver"))))
