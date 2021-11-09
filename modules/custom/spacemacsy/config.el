;;; custom/spacemacsy/config.el -*- lexical-binding: t; -*-

(after! hydra
 (map! :leader :n "zf" #'+hydra/text-zoom/body))

;;;;;;;;;;;;;
;; windows ;;
;;;;;;;;;;;;;

(use-package! winum
  :config
  (dolist (wn (seq-map 'number-to-string (number-sequence 0 9)))
    (let ((f (intern (concat "winum-select-window-" wn))))
      (map! :n (concat "s-" wn) f)
      (map! :leader :n wn f)))
  (winum-mode))

(map! :leader
      :prefix "w"
      :n "m" #'toggle-maximize-buffer
      :n "|" #'delete-other-windows-vertically
      :n "_" #'delete-other-windows-horizontally)

;;;;;;;;;;;
;; sexps ;;
;;;;;;;;;;;

(defun sp-wrap-sexp ()
  (interactive)
  (sp-wrap-with-pair "("))

(defun sp-reindent ()
  (interactive)
  (save-excursion
    (er/expand-region 2)
    (evil-indent
     (region-beginning)
     (region-end))))

(map! :leader
      :prefix "k"
      :n "w" #'sp-wrap-sexp
      :n "W" #'sp-unwrap-sexp
      :n "r" #'sp-raise-sexp
      :n "y" #'sp-copy-sexp
      :n "dx" #'sp-kill-sexp
      :n "s" #'sp-forward-slurp-sexp
      :n "b" #'sp-forward-barf-sexp
      :n "=" #'sp-reindent)

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp)

(map! :leader :n "bs" #'doom/switch-to-scratch-buffer)

;;;;;;;;;;;;;;;;
;; workspaces ;;
;;;;;;;;;;;;;;;;

(map! :leader "TAB" #'alternate-buffer)

;; (map! :leader
;;       "TAB" #'alternate-buffer
;;       "l" doom-leader-workspace-map
;;       "ll" #'+workspace/switch-to)

(map! :leader :n "ry" #'yank-from-kill-ring)


;;;;;;;;;;;;
;; editor ;;
;;;;;;;;;;;;

(map! :leader
      :n "v" #'er/expand-region
      :n ";" #'evilnc-comment-or-uncomment-lines
      :n "jj" #'avy-goto-char-timer)


;;;;;;;;;;;;;
;; buffers ;;
;;;;;;;;;;;;;

(map! "s-b" #'consult-buffer)

(map! :leader
      :prefix "b"
      "m" #'switch-to-messages-buffer
      "d" #'kill-this-buffer)


(map! :leader :prefix "b"
      "s-d" #'spacemacs/kill-matching-buffers-rudely)

;;;;;;;;;;;
;; files ;;
;;;;;;;;;;;

(map! :leader "fe" nil)

(map! :leader
      :prefix "fe"
      "d" #'doom/goto-private-config-file
      "i" (lambda ()
            (interactive)
            (dired doom-emacs-dir)))

(map! :leader
      :prefix "w"
      "D" #'ace-delete-window)

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(map! :leader
      "gj" #'evil-show-jumps
      "sj" #'imenu)


;; fix for smartparens. Doom's default module does things like skipping pairs if
;; one typed at the beginning of the word.
(dolist (brace '("(" "{" "["))
      (sp-pair brace nil
               :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
               :unless '(sp-point-before-same-p)))

(sp-local-pair sp-lisp-modes "(" ")"
               :wrap ")"
               :unless '(:rem sp-point-before-same-p))
