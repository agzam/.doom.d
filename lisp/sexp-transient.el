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
  ["Auxiliary keys"
   :hide always
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'sexp-transient
      ;; sets up 'special' keys for this transient,
      ;;
      ;; - for the string nominal of the key - calls the command that
      ;;   normally binds to it, exiting the transient
      ;;
      ;; - alternatively, can be a list with the key, transient flag,
      ;; and the command - if you want to explicitly
      ;; override the one that normally binds to the key.
      (thread-last
        '("p" "P" "C-;" "g" "G"
          "SPC" "," ":" "M-x" "M-:" "`" "C-h"
          "[" "]"
          ("C-l" t) ("C-e" t) ("C-y" t)
          ("s" nil evil-surround-region)
          ("%" t evilmi-jump-items)
          ("o" t evilmi-jump-items)
          ("0" t evil-beginning-of-line) ("$" t)
          ("f" t) ("F" t) ("t" t) ("T" t)
          ("/" t))
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
                                  (or (lookup-key evil-motion-state-map (kbd key))
                                      (lookup-key evil-visual-state-map (kbd key))
                                      (lookup-key evil-normal-state-map (kbd key))
                                      (lookup-key global-map (kbd key))))
                               (general--simulate-keys nil key)))))
                  (desc (format "%s" key)))
             (list key desc cmd :transient transient?)))))))]
  ["sexp"
   [("w" "wrap" sp-wrap-sexp :transient t)
    ("W" "unwrap" sp-unwrap-sexp :transient t)
    ("=" "reindent" sp-reindent :transient t)]
   [("r" "raise" sp-raise-sexp :transient t)
    ("c" "convolute" sp-convolute-sexp :transient t)
    ("t" "transpose" sp-transpose-sexp :transient t)]
   [("|" "split" sp-split-sexp :transient t)
    ("J" "join" sp-join-sexp :transient t)]
   [("n n" "narrow" (lambda ()
                      (interactive)
                      (save-mark-and-excursion
                        (mark-sexp)
                        (narrow-to-region
                         (region-beginning)
                         (region-end))))
     :transient t)
    ("n w" "widen" widen :transient t)]
   [("." "slurp" sp-forward-slurp-sexp :transient t)
    (">" "barf" sp-forward-barf-sexp :transient t)
    ("," "left slurp" sp-backward-slurp-sexp :transient t)
    ("<" "left barf" sp-backward-barf-sexp :transient t)]
   [("d x" "kill" sp-kill-sexp)
    ("y" "copy" sp-copy-sexp)
    ("v" "select" (lambda () (interactive)
                    (expreg-expand)
                    (expreg-transient)))
    ("u" "undo" evil-undo :transient t)]])

(provide 'sexp-transient)
;;; sexp-transient.el ends here
