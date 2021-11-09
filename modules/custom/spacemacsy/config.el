;;; custom/spacemacsy/config.el -*- lexical-binding: t; -*-

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

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp)

(map! :leader
      "TAB" #'alternate-buffer
      "v" #'er/expand-region
      ";" #'evilnc-comment-or-uncomment-lines
      "a" nil ; liberate the top level binding for other things
      (:prefix "z"
       "f" #'+hydra/text-zoom/body)

      (:prefix "w"
       "m" #'toggle-maximize-buffer
       "|" #'delete-other-windows-vertically
       "_" #'delete-other-windows-horizontally
       "D" #'ace-delete-window)

      (:prefix "k"
       "w" #'sp-wrap-sexp
       "W" #'sp-unwrap-sexp
       "r" #'sp-raise-sexp
       "y" #'sp-copy-sexp
       "dx" #'sp-kill-sexp
       "s" #'sp-forward-slurp-sexp
       "b" #'sp-forward-barf-sexp
       "=" #'sp-reindent)

      (:prefix "b"
       :desc "scratch" "s" #'doom/switch-to-scratch-buffer
       :desc "Messages" "m" #'switch-to-messages-buffer
       "d" #'kill-this-buffer
       "s-d" #'spacemacs/kill-matching-buffers-rudely)

      (:prefix "r"
       "y" #'yank-from-kill-ring)

      (:prefix "j"
       "j" #'avy-goto-char-timer)

      (:prefix "g"
       "j" #'evil-show-jumps
       "s" #'magit-status
       "f" #'magit-file-dispatch)

      (:prefix "s"
       "j" #'imenu)

      (:prefix "t"
       "w" #'toggle-visual-line-navigation)

      (:prefix "f"
        "e" nil
       (:prefix "e"
        "d" #'doom/goto-private-config-file
        "i" (lambda () (interactive) (dired doom-emacs-dir))))
      (:prefix "a"
       "a" #'embark-act
       (:prefix "g"
        "h" #'gh-notify)))

(use-package! winum
  :config
  (dolist (wn (seq-map 'number-to-string (number-sequence 0 9)))
    (let ((f (intern (concat "winum-select-window-" wn))))
      (map! :n (concat "s-" wn) f)
      (map! :leader :n wn f)))
  (winum-mode))

(map! "s-b" #'consult-buffer)

;; fix for smartparens. Doom's default module does things like skipping pairs if
;; one typed at the beginning of the word.
(dolist (brace '("(" "{" "["))
      (sp-pair brace nil
               :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
               :unless '(sp-point-before-same-p)))

(sp-local-pair sp-lisp-modes "(" ")"
               :wrap ")"
               :unless '(:rem sp-point-before-same-p))

;; Change the cursor color in emacs state. We do it this roundabout way
;; to ensure changes in theme doesn't break these colors.
(add-hook! '(doom-load-theme-hook doom-init-modules-hook)
  (defun +evil-update-cursor-color-h ()
    (put 'cursor 'evil-emacs-color "SkyBlue2")
    (put 'cursor 'evil-normal-color "DarkGoldenrod2")))

;; often conflicts with doom-local-leader
(after! evil-maps (unbind-key (kbd ",") evil-motion-state-map))
