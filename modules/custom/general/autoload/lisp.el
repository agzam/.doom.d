;;; custom/general/autoload/lisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sp-reindent ()
  (interactive)
  (save-excursion
    (er/expand-region 2)
    (evil-indent
     (region-beginning)
     (region-end))))

;;;###autoload
(defun sp-wrap-sexp ()
  (interactive)
  (sp-wrap-with-pair "("))
