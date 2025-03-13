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
;; Package-Requires: ((emacs "30"))
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

;;;###autoload
(defun sp-evil-sexp-go-back ()
  (interactive)
  (let* ((closing? (looking-at-p "[])}]"))
         (enc (sp-get-sexp))
         (b (when enc (sp-get enc :beg)))
         (e (when enc (sp-get enc :end)))
         (same-line? (when enc (and
                                (equal (line-number-at-pos b)
                                       (line-number-at-pos e))
                                (< 3 (- e b))))))
    (if (and closing? same-line?) (goto-char b)
      (progn
        (backward-char)
        (search-backward-regexp "[])}]\\|[[({]")))))

;;;###autoload
(defun sp-evil-sexp-go-forward ()
  (interactive)
  (let* ((curr (point))
         (opening? (looking-at-p "[[({]"))
         (enc (sp-get-sexp))
         (b (when enc (sp-get enc :beg)))
         (e (when enc (sp-get enc :end)))
         (same-line? (when enc (and
                                (equal (line-number-at-pos b)
                                       (line-number-at-pos e))
                                (< 3 (- e b))))))
    (if (and opening? same-line?)
        (goto-char e)
      (progn
        (forward-char)
        (unless (eq curr (search-forward-regexp "[[({]\\|[])}]"))
          (backward-char))))))

(transient-define-prefix sexp-transient ()
  "rule the parens"
  ["Navigation"
   :hide always
   [("k" "left" sp-evil-sexp-go-back :transient t)
    ("j" "right" sp-evil-sexp-go-forward :transient t)]]
  ["sexp"
   [("w" "wrap" sp-wrap-sexp :transient t)]])

(provide 'sexp-transient)
;;; sexp-transient.el ends here
