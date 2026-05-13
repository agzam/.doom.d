;;; custom/elisp/autoload/eval.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sp-eval-current-sexp (&optional arg)
  "Eval current sexp."
  (interactive "p")
  (save-excursion
    (let ((evil-move-beyond-eol t))
      (when (looking-at-p "[[({]")
        (forward-char))
      (sp-up-sexp)
      (call-interactively 'eros-eval-last-sexp))))

;;;###autoload
(defun pp-eval-current (&optional arg)
  "Like pp-eval-last-sexp, but for current"
  (interactive "p")
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (goto-char
       (plist-get (or (sp-get-enclosing-sexp)
                      (sp-get-expression)) :end))
      (call-interactively 'pp-eval-last-sexp))))

;;; borrowed from: http://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
;;;###autoload
(defun sharp-quote ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

;;;###autoload
(defun with-editor-eval ()
  "Evaluates current form and pops a buffer with the results.
Usually, the results of evaluation go into *Messages* buffer,
this doesn't change that, it simply copies the relevant log into
its own buffer."
  (interactive)
  (let ((last-pos (with-current-buffer "*Messages*"
                    (point-max))))
    (eval-current-form-sp)
    (let ((log (with-current-buffer "*Messages*"
                 (buffer-substring last-pos (point-max)))))
      (with-current-buffer (get-buffer-create "*eval*")
        (erase-buffer)
        (insert log)
        (switch-to-buffer-other-window (current-buffer))))))
