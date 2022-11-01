;;; custom/general/autoload/lisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sp-reindent ()
  (interactive)
  (save-mark-and-excursion
    (er/expand-region 2)
    (if (bound-and-true-p lsp-mode)
        (lsp--indent-lines
         (region-beginning)
         (region-end))
      (evil-indent
       (region-beginning)
       (region-end)))))

;;;###autoload
(defun sp-wrap-sexp ()
  (interactive)
  (sp-wrap-with-pair "("))
