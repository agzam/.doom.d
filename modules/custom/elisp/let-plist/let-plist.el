;;; custom/elisp/let-plist/let-plist.el -*- lexical-binding: t; -*-

;;; let-plist.el --- Let-bind for plists -*- lexical-binding: t -*-

;; The following snippet was borrowed from
;; https://emacs.stackexchange.com/questions/45581/equivalent-of-let-alist-for-plists

(require 'let-alist)

(defun let-plist--list-to-sexp (list var)
  "Turn symbols LIST into recursive calls to `plist-get' on VAR."
  `(plist-get ,(if (cdr list)
                   (let-plist--list-to-sexp (cdr list) var)
                 var)
              ',(intern (concat ":" (symbol-name (car list))))))

(defun let-plist--access-sexp (symbol variable)
  "Return a sexp used to access SYMBOL inside VARIABLE."
  (let* ((clean (let-alist--remove-dot symbol))
         (name (symbol-name clean)))
    (if (string-match "\\`\\." name)
        clean
      (let-plist--list-to-sexp
       (mapcar #'intern (nreverse (split-string name "\\.")))
       variable))))

(defmacro let-plist (plist &rest body)
  "Let-bind dotted symbols to their cdrs in PLIST and execute BODY.

Similar to `let-alist'."
  (declare (indent 1))
  (let ((var (make-symbol "plist")))
    `(let ((,var ,plist))
       (let ,(mapcar (lambda (x) `(,(car x) ,(let-plist--access-sexp (car x) var)))
                     (delete-dups (let-alist--deep-dot-search body)))
         ,@body))))


(provide 'let-plist)

;;; let-plist.el ends here
