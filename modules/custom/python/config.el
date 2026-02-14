;;; custom/python/config.el -*- lexical-binding: t; -*-

(use-package! python
  :defer t
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :config
  (add-hook! (python-ts-mode python-mode)
             #'lsp!
             #'+python-mode-lookup-handlers
             (defun activate-python-dash-docset-h ()
               (dash-docs-activate-docset "Python 3"))
             (defun set-python-trace-definition-h ()
               (setq-local magit-log-trace-definition-function
                           #'magit-python-which-function))))

(use-package! lsp-pyright
  :defer t
  :init
  ;; important: this needs to be set before the package loads
  (setopt lsp-pyright-multi-root nil)
  :config
  (setopt
   ;; don't forget to:
   ;; uv tool install basedpyright
   ;; uv tool install ruff
   lsp-pyright-langserver-command "basedpyright"
   lsp-pyright-venv-path "."
   lsp-pyright-venv-directory ".venv"
   lsp-ruff-python-path "python"
   python-shell-interpreter "python")

  (setopt lsp-pyright-type-checking-mode "off")

  (lsp-register-custom-settings
   `(("basedpyright.analysis.typeCheckingMode" lsp-pyright-type-checking-mode)))

  (lsp-dependency
   'pyright
   `(:system ,(executable-find "basedpyright-langserver")))

  (map!
   :map (python-mode-map python-ts-mode-map)
   "C-c C-n" #'python-edit-imports
   (:localleader
    (:prefix ("i" . "insert")
             "p" #'python-insert-ipdb)
    (:prefix ("f" . "format")
             "r" #'python-format)
    (:prefix ("c" . "convert")
             "j" #'python-to-json
             "e" #'python-to-edn)
    (:prefix ("e" . "errors")
             "f" #'python-fix-all)
    (:prefix ("f" . "format")
             "i" #'python-fix-imports))))
