;;; custom/completion/autoload/yasnippet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun yas-completing-prompt-a (fn &optional prompt choices display-fn _)
  "`yas-choose-value' by default calls `completing-read' with `require-match'
and that is slightly annoying, as it doesn't let you type an option
that is not in a list. Let's hack it, so there's no match required."
  (cl-letf (((symbol-function 'completing-read*)
             (lambda (prompt coll _ _ _ _)
               (completing-read prompt coll))))
    (funcall fn prompt choices display-fn (symbol-function 'completing-read*))))

;;;###autoload
(defun yas-expand-to-completion-h ()
      (setq-local completion-at-point-functions
                  (cons #'yasnippet-capf completion-at-point-functions)))

;;;###autoload
(defun temporarily-disable-smart-parens ()
  "Disables and re-enables smartparens.
Smartparens can get in a way of yasnippet while expanding
snippets containing parentheses."
  (when smartparens-mode
    (turn-off-smartparens-mode)
    (run-with-timer
     0.1 nil
     #'turn-on-smartparens-mode)))
