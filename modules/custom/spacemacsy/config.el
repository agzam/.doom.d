;;; custom/spacemacsy/config.el -*- lexical-binding: t; -*-

(use-package! hydra
  :config
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

(map! :leader :n "wm" #'toggle-maximize-buffer)

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

(map! :leader
      "TAB" #'alternate-buffer
      "l" doom-leader-workspace-map
      "ll" #'+workspace/switch-to)

(map! :leader :n "ry" #'yank-from-kill-ring)


;;;;;;;;;;;;
;; editor ;;
;;;;;;;;;;;;

(map! :leader :n "v" #'er/expand-region)

(map! :leader :n ";" #'evilnc-comment-or-uncomment-lines)

;;;;;;;;;;;;;
;; buffers ;;
;;;;;;;;;;;;;

(map! "s-b" #'consult-buffer)


(map! :leader :prefix "b"
      "s-d" #'spacemacs/kill-matching-buffers-rudely)
