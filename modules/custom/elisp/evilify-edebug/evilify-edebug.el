;;; evilify-edebug.el --- Evil bindings for edebug -*- lexical-binding: t -*-

;;; Based on emacs-evil/evil-collection by James Nguyen <james@jojojames.com>
;; https://github.com/emacs-evil/evil-collection/blob/master/modes/edebug/evil-collection-edebug.el
(require 'edebug)
(require 'evil-collection)

(defun evilify-edebug-setup ()
  "Set up `evil' bindings for `edebug'."
  (evil-set-initial-state 'edebug-mode 'normal)

  (add-hook 'edebug-mode-hook #'evil-normalize-keymaps)

  (evil-collection-define-key nil 'edebug-mode-map
    "g" nil
    "G" nil
    "h" nil
    "v" nil)

  ;; FIXME: Seems like other minor modes will readily clash with `edebug'.
  ;; `lispyville' and `edebug' 's' key?
  (evil-collection-define-key 'normal 'edebug-mode-map
    ;; control
    "v" nil
    "s" 'edebug-step-mode
    "\C-n" 'edebug-next-mode
    "go" 'edebug-go-mode
    "gO" 'edebug-Go-nonstop-mode
    "t" 'edebug-trace-mode
    "T" 'edebug-Trace-fast-mode
    "c" 'edebug-continue-mode
    "C" 'edebug-Continue-fast-mode

    "f" 'edebug-forward-sexp
    "H" 'edebug-goto-here
    "I" 'edebug-instrument-callee
    "\C-i" 'edebug-step-in
    "o" 'edebug-step-out

    ;; quit
    "q" 'top-level
    "Q" 'edebug-top-level-nonstop
    "a" 'abort-recursive-edit
    "S" 'edebug-stop

    ;; breakpoints
    "b" 'edebug-set-breakpoint
    "u" 'edebug-unset-breakpoint
    "B" 'edebug-next-breakpoint
    "x" 'edebug-set-conditional-breakpoint
    "X" 'edebug-set-global-break-condition

    ;; evaluation
    "r" 'edebug-previous-result
    "e" 'edebug-eval-expression
    (kbd "C-x C-e") 'edebug-eval-last-sexp
    "EL" 'edebug-visit-eval-list

    ;; views
    "WW" 'edebug-where
    "p" 'edebug-bounce-point
    "P" 'edebug-view-outside ;; same as v
    "WS" 'edebug-toggle-save-windows

    ;; misc
    "g?" 'edebug-help
    "d" 'edebug-backtrace

    "-" 'negative-argument

    ;; statistics
    "=" 'edebug-temp-display-freq-count

    ;; GUD bindings
    (kbd "C-c C-s") 'edebug-step-mode
    (kbd "C-c C-n") 'edebug-next-mode
    (kbd "C-c C-c") 'edebug-go-mode

    (kbd "C-x SPC") 'edebug-set-breakpoint
    (kbd "C-c C-d") 'edebug-unset-breakpoint
    (kbd "C-c C-t") (lambda () (interactive) (edebug-set-breakpoint t))
    (kbd "C-c C-l") 'edebug-where))

;;;###autoload
(defun edebug-eval-current-form-sp (&optional arg)
  "Just like 'eval-current-form-sp' but work in edebug context."
  (interactive "p")
  (let ((evil-move-beyond-eol t))
    (save-excursion
      (goto-char
       (plist-get (or (sp-get-enclosing-sexp)
                      (sp-get-expression)) :end))
      (call-interactively #'edebug-eval-last-sexp))))

(provide 'evilify-edebug)

;;; evilify-edebug.el ends here
