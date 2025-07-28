;;; sexp-transient.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: March 12, 2025
;; Modified: March 12, 2025
;; Version: 0.0.1
;; Keywords: lisp transient evil
;; Package-Requires: ((emacs "29.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'smartparens)
(require 'transient)
(require 'subr-x)
(require 'general)
(require 'avy)
(require 'edit-indirect)

(defun transient-bypass-keys (prefix key-specs)
  "Create a bunch of keys (transient suffixes) as bypas keys.
For special handling in a transient PREFIX.

Sometimes you want a transient where pressing e.g., the backspace key
does exactly the same thing as you press it outside of the transient,
taking into the account major and minor mode maps, etc.

Every key spec in KEY-SPECS list can be, either:

- a string, to invoke the command that normally binds to the key,
  while exiting the transient.

- or, a list with the key string nominal and the transient flag.
  Optionally, the explicit command to call."
  (transient-parse-suffixes
   prefix
   (mapcar
    (lambda (key-map)
      (let* ((key (if (stringp key-map) key-map (car key-map)))
             (explicit-cmd (ignore-errors (nth 2 key-map)))
             (transient? (and (listp key-map) (cadr key-map)))
             (cmd (or explicit-cmd
                      (lambda ()
                        (interactive)
                        (if transient?
                            (call-interactively
                             (or
                              (lookup-key evil-normal-state-map (kbd key))
                              (lookup-key evil-motion-state-map (kbd key))
                              (lookup-key evil-visual-state-map (kbd key))
                              (lookup-key (current-local-map) (kbd key))
                              (lookup-key global-map (kbd key))))
                          (general--simulate-keys nil key)))))
             (desc (format "%s" key)))
        (list key desc cmd :transient transient?)))
    key-specs)))


;;;###autoload
(defun sp-evil-sexp-go-back ()
  "Find previous sexp."
  (interactive)
  (backward-char)
  (search-backward-regexp "[])}]\\|[[({]"))

;;;###autoload
(defun sp-evil-sexp-go-forward ()
  "Find next sexp."
  (interactive)
  (let* ((curr (point)))
    (forward-char)
    (unless (eq curr (search-forward-regexp "[[({]\\|[])}]"))
      (backward-char))))

;;;###autoload
(defun sp-narrow-to-current-sexp ()
  "Narrow screen to current sexp."
  (interactive)
  (save-mark-and-excursion
    (sp-beginning-of-sexp)
    (backward-char)
    (sp-mark-sexp)
    (narrow-to-region
     (region-beginning)
     (region-end))))

;;;###autoload
(defun sp-edit-indirect-current-sexp ()
  "Edit current sexp in an indirect buffer."
  (interactive)
  (let* ((reg (save-mark-and-excursion
               (sp-beginning-of-sexp)
               (backward-char)
               (sp-mark-sexp)
               (list (region-beginning)
                     (region-end))))
         (edit-indirect-guess-mode-function
          (lambda (pb _ _)
            (funcall (with-current-buffer pb major-mode)))))
    (funcall-interactively
     #'edit-indirect-region
     (car reg) (cadr reg) t)))

;;;###autoload
(defun avy-goto-beg-sexp ()
  "Use avy to jump to a beginning of sexp."
  (interactive)
  (let* ((avy-command this-command) ; for look up in avy-orders-alist
         (avy-style 'post))
    (avy-jump "(+\\|\\[+\\|{+" :window-flip nil)))

(add-to-list 'avy-orders-alist '(avy-goto-beg-sexp . avy-order-closest))

;;;###autoload
(defun avy-goto-end-sexp ()
  "Use avy to jump to a end of sexp."
  (interactive)
  (let* ((avy-command this-command)
         (avy-style 'post))
    (avy-jump "\\([^])}>]+\\)[])}]+"
              :window-flip nil
              :action (lambda (pt)
                        (goto-char pt)
                        (re-search-forward "[])}]+" nil t 1)))))

(add-to-list 'avy-orders-alist '(avy-goto-end-sexp . avy-order-closest))

;;;###autoload
(defun sp-eval-current-in-mode ()
  "Evals current sexp in its dedicated mode evaluator."
  (interactive)
  (cond
   ((derived-mode-p 'clojure-mode)
    (call-interactively #'cider-eval-sexp-at-point*))
   (t (call-interactively #'sp-eval-current-sexp))))

(defun sp-pp-eval-current-in-mode ()
  "Eval & pretty-print sexp."
  (interactive)
  (cond
   ((derived-mode-p 'clojure-mode)
    (call-interactively #'cider-pprint-eval-sexp-at-point))
   (t (call-interactively #'pp-eval-current))))

(transient-define-prefix sexp-transient ()
  "rule the parens"
  ["Navigation"
   :hide always
   [("k" "k" sp-evil-sexp-go-back :transient t)
    ("j" "j" sp-evil-sexp-go-forward :transient t)
    ("h" "h" sp-backward-parallel-sexp :transient t)
    ("l" "l" sp-forward-parallel-sexp :transient t)
    ("<down>" "j" evil-next-visual-line :transient t)
    ("<up>" "k" evil-previous-visual-line :transient t)
    ("<left>" "h" evil-backward-char :transient t)
    ("<right>" "l" evil-forward-char :transient t)]]
  ["bypass keys"
   :hide always
   :setup-children
   (lambda (_)
     (transient-bypass-keys
      'sexp-transient
      '("p" "P" "C-;" "g" "G"
        "SPC" "," ":" "M-x" "M-:" "`"
        "s-k" "s-]" "s-j" "s-]"
        "[" "]"
        ("C-l" t) ("C-e" t) ("C-y" t)
        ("s" nil evil-surround-region)
        ("%" t)
        ("o" t evilmi-jump-items)
        ("0" t) ("$" t)
        ("f" t) ("F" t) ("t" t) ("T" t)
        ("/" t))))]
  ["sexp"
   [("a" "avy" avy-goto-beg-sexp :transient t)
    ("A" "avy" avy-goto-end-sexp :transient t)]
   [("w" "wrap" sp-wrap-sexp :transient t)
    ("W" "unwrap" sp-unwrap-sexp :transient t)
    ("=" "reindent" sp-reindent :transient t)]
   [("r" "raise" raise-sexp :transient t)
    ("c" "convolute" sp-convolute-sexp :transient t)
    ("t" "transpose" sp-transpose-sexp :transient t)]
   [("|" "split" sp-split-sexp :transient t)
    ("J" "join" sp-join-sexp :transient t)]
   [("n n" "narrow" sp-narrow-to-current-sexp :transient t)
    ("n w" "widen" widen :transient t)
    ("E" "edit" sp-edit-indirect-current-sexp :transient t)]
   [("M-l" "slurp" sp-forward-slurp-sexp :transient t)
    ("M-h" "barf" sp-forward-barf-sexp :transient t)
    ("M-S-h" "left slurp" sp-backward-slurp-sexp :transient t)
    ("M-S-l" "left barf" sp-backward-barf-sexp :transient t)]
   [("d x" "kill" sp-kill-sexp)
    ("y" "copy" sp-copy-sexp)
    ("v" "select" (lambda () (interactive)
                    (expreg-expand)
                    (expreg-transient)))
    ("u" "undo" evil-undo :transient t)]
   [("e c" "eval current" sp-eval-current-in-mode)
    ("e p" "pprint" sp-pp-eval-current-in-mode)
    ("e ;" "eval to comment"
     cider-pprint-eval-last-sexp-to-comment
     :if (lambda () (derived-mode-p 'clojure-mode)))
    ("#" "ignore" clojure-toggle-ignore
     :if (lambda () (derived-mode-p 'clojure-mode)))]]
  ["Clojure"
   :if (lambda () (derived-mode-p 'clojure-mode))
   :hide (lambda () (not transient-show-common-commands))
   [("> SPC" "->" lsp-clojure-thread-first :transient t)
    (">>" "->>" lsp-clojure-thread-last :transient t)
    ("<" "un-thread" lsp-clojure-unwind-thread :transient t)
    ("; c" "wrap comment" clojure-wrap-rich-comment)]])

(provide 'sexp-transient)
;;; sexp-transient.el ends here
