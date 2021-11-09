;;; custom/elisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'paradox-menu-mode--set-keys "custom/elisp/autoload" nil t)
(defun paradox-menu-mode--set-keys ()
  (map! :localleader
        :map paradox-menu-mode-map
        "r" #'paradox-filter-regexp
        "f" #'hydra-paradox-filter/body))

;;;###autoload (autoload 'eval-current-form-sp "custom/elisp/autoload" nil t)
(defun eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (goto-char
       (plist-get (or (sp-get-enclosing-sexp)
                      (sp-get-expression)) :end))
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
