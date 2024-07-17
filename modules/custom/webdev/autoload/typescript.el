;;; custom/webdev/autoload/typescript.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +typescript-mode-lookup-handlers ()
  (set-lookup-handlers! '(typescript-mode
                          typescript-ts-mode)
    :definition #'+lsp-lookup-definition-handler
    :references #'+lsp-lookup-references-handler
    :implementations '(lsp-find-implementation :async t)
    :type-definition #'lsp-find-type-definition
    :documentation #'+consult-dash-doc))

;;;###autoload
(defun activate-ts-dash-docsets-h ()
  (dolist (docset '("TypeScript" "CSS"))
    (dash-docs-activate-docset docset)))
