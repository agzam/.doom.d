;;; common/general/config.el -*- lexical-binding: t; -*-

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
      :i "M-h" #'sp-forward-barf-sexp
      "s-b" #'consult-buffer
      "s-=" #'text-scale-increase
      "s--" #'text-scale-decrease)

(put 'narrow-to-region 'disabled nil)

(map! :leader
      "TAB" #'alternate-buffer
      "v" #'er/expand-region
      ";" #'evilnc-comment-or-uncomment-lines
      "a" nil ; liberate the top level binding for other things

      "nr" #'narrow-to-region
      "nf" #'narrow-to-defun
      "nw" #'widen
      "nR" #'narrow-to-region-indirect-buffer
      "nF" #'narrow-to-defun-indirect-buffer

      (:prefix "z"
       "f" #'+hydra/text-zoom/body)

      (:prefix "w"
       "m" #'toggle-maximize-buffer
       "M" #'ace-swap-window
       "W" #'ace-window
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
       "j" #'avy-goto-char-timer
       "x" #'xwidget-webkit-url-get-create)

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

(map! :localleader :map xwidget-webkit-mode-map "x" #'kill-current-buffer)

(after! smartparens
 ;; fix for smartparens. Doom's default module does things like skipping pairs if
 ;; one typed at the beginning of the word.
 (dolist (brace '("(" "{" "["))
   (sp-pair brace nil
            :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
            :unless '(sp-point-before-same-p)))

 (sp-local-pair sp-lisp-modes "(" ")"
                :wrap ")"
                :unless '(:rem sp-point-before-same-p)))

;; Change the cursor color in emacs state. We do it this roundabout way
;; to ensure changes in theme doesn't break these colors.
(add-hook! '(doom-load-theme-hook doom-init-modules-hook)
  (defun +evil-update-cursor-color-h ()
    (put 'cursor 'evil-emacs-color "SkyBlue2")
    (put 'cursor 'evil-normal-color "DarkGoldenrod2")))

(after! evil-maps
  ;; often conflicts with doom-local-leader
  ;; (unbind-key (kbd ",") evil-motion-state-map)
  (unbind-key (kbd "C-u") evil-motion-state-map))

(use-package! winum
  :config
  (dolist (wn (seq-map 'number-to-string (number-sequence 0 9)))
    (let ((f (intern (concat "winum-select-window-" wn))))
      (map! :n (concat "s-" wn) f)
      (map! :leader :n wn f)))
  (winum-mode))

(use-package! embark-consult
  :config)

(use-package! fasd
  :config
  (map! :leader "fad" #'fasd-find-file)
  (global-fasd-mode +1))

(use-package! consult-company
  :config
  (map! :map company [remap completion-at-point] #'consult-company))

(use-package! vertico-posframe
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-mode +1)
  (setq marginalia-margin-threshold 300))

;; Add vertico extensions load path
(add-to-list 'load-path (format "%sstraight/build-%s/vertico/extensions/" (file-truename doom-local-dir) emacs-version))

(use-package! vertico-repeat
  :after vertico
  :config
  (map! :leader "rl" #'vertico-repeat)
  (add-hook! 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package! vertico-quick
  :after vertico
  :config
  (map! :map vertico-map "C-'" #'vertico-quick-insert))

(use-package! vertico-directory
  :after vertico
  :config
  (map! :map vertico-map "C-h" #'vertico-directory-delete-word))

(use-package! vertico-grid
  :after vertico
  :config
  (map! :map vertico-map
        "C-c g" #'vertico-grid-mode
        "M-h" #'vertico-grid-left
        "M-l" #'vertico-grid-right)
  (add-hook! 'minibuffer-exit-hook
    (defun vertico-grid-mode-off ()
      (vertico-grid-mode -1))))

(use-package! vertico-buffer :after vertico)

(after! vertico
  (map! :map vertico-map
        "C-e" #'vertico-scroll-up
        "C-y" #'vertico-scroll-down
        ;; unbind universal argument
        "C-u" nil))

(use-package! spacehammer
  :config
  (add-hook! spacehammer/edit-with-emacs #'on-spacehammer-edit-with-emacs)
  (add-hook! spacehammer/before-finish-edit-with-emacs #'spacehammer-before-finish-edit-with-emacs))

(after! ibuf-ext
  (setq
   ibuffer-old-time 8 ; buffer considered old after that many hours
   ibuffer-group-buffers-by 'projects
   ibuffer-expert t
   ibuffer-show-empty-filter-groups nil)

  (define-ibuffer-filter unsaved-file-buffers
      "Toggle current view to buffers whose file is unsaved."
    (:description "file is unsaved")
    (ignore qualifier)
    (and (buffer-local-value 'buffer-file-name buf)
         (buffer-modified-p buf)))

  (define-ibuffer-filter file-buffers
      "Only show buffers backed by a file."
    (:description "file buffers")
    (ignore qualifier)
    (buffer-local-value 'buffer-file-name buf))

  (map! :map ibuffer-mode-map
        :n "su" #'ibuffer-filter-by-unsaved-file-buffers
        :n "sF" #'ibuffer-filter-by-file-buffers))
