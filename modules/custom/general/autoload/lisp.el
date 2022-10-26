;;; custom/general/autoload/lisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sp-reindent ()
  (interactive)
  (save-excursion
    (er/expand-region 2)
    (if lsp-mode
        (lsp-format-region
         (region-beginning)
         (region-end))
      (evil-indent
       (region-beginning)
       (region-end)))))

;;;###autoload
(defun sp-wrap-sexp ()
  (interactive)
  (sp-wrap-with-pair "("))
