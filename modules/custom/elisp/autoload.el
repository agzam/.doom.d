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
(defun erase-messages-buffer ()
  (interactive)
  (with-current-buffer "*Messages*"
    (read-only-mode -1)
    (erase-buffer)
    (read-only-mode 1)))

;;;###autoload
(defun +switch-to-messages-buffer-other-window ()
  (interactive)
  (setq +last-known-elisp-buffer (current-buffer))
  (if-let ((mwin (get-buffer-window (messages-buffer))))
      (select-window mwin)
    (progn
      (+evil/window-vsplit-and-follow)
      (switch-to-messages-buffer))))

;;;###autoload
(defun +switch-to-last-elisp-buffer ()
  (interactive)
  (when (boundp '+last-known-elisp-buffer)
    (select-window
     (get-buffer-window
      +last-known-elisp-buffer))))

;;;###autoload
(defun +hide-messages-window ()
  (interactive)
  (when-let ((mw (get-buffer-window (messages-buffer))))
    (delete-window mw)))
