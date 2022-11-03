;;; common/general/config.el -*- lexical-binding: t; -*-

(put 'narrow-to-region 'disabled nil)

;; Change the cursor color in emacs state. We do it this roundabout way
;; to ensure changes in theme doesn't break these colors.
(add-hook! '(doom-load-theme-hook doom-init-modules-hook)
  (defun +evil-update-cursor-color-h ()
    (put 'cursor 'evil-emacs-color "SkyBlue2")
    (put 'cursor 'evil-normal-color "DarkGoldenrod2")
    (posframe-delete-all)))

(use-package winum
  :after-call doom-switch-window-hook
  :config
  (setq winum-scope 'frame-local)
  (winum-mode +1)
  (dolist (wn (seq-map 'number-to-string (number-sequence 0 9)))
    (let ((f (intern (concat "winum-select-window-" wn)))
          (k (concat "s-" wn)))
      (map! :n k f)
      (map! :leader :n wn f)
      (global-set-key (kbd k) f))))

(use-package! fasd
  :commands fasd-find-file
  :defer t
  :config
  (global-fasd-mode +1))

;;;;;;;;;;;;;;;;;;;
;; vertico stuff ;;
;;;;;;;;;;;;;;;;;;;

;; Add vertico extensions load path
(add-to-list 'load-path (format "%sstraight/build-%s/vertico/extensions/" (file-truename doom-local-dir) emacs-version))

(use-package! vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (setq
   vertico-posframe-global t
   vertico-posframe-height 23
   vertico-posframe-width 200
   marginalia-margin-threshold 500)
  (vertico-posframe-mode +1)

  ;; disable and restore posframe when emacslient connects in terminal
  (add-hook! 'after-make-frame-functions
    (defun disable-vertico-posframe-in-term-h (frame)
      (when (and (not (display-graphic-p frame))
                 (bound-and-true-p vertico-posframe-mode))
        (vertico-posframe-mode -1)
        (setq vertico-posframe-restore-after-term-p t))))

  (add-hook! 'delete-frame-functions
    (defun restore-vertico-posframe-after-term-h (frame)
      (when (bound-and-true-p vertico-posframe-restore-after-term-p)
        (vertico-posframe-mode +1))))

  ;; fixing "Doesn't properly respond to C-n"
  ;; https://github.com/tumashu/vertico-posframe/issues/11
  (defadvice! vertico-posframe--display-no-evil (fn _lines)
    :around #'vertico-posframe--display
    (evil-mode -1)
    (funcall-interactively fn _lines))

  (add-hook! 'minibuffer-exit-hook #'evil-mode))

(use-package! vertico-repeat
  :after vertico
  :config
  (add-hook! 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package! vertico-quick
  :after vertico)

(use-package! vertico-directory
  :after vertico)

(use-package! vertico-grid
  :after vertico
  :config
  (add-hook! 'minibuffer-exit-hook
    (defun vertico-grid-mode-off ()
      (vertico-grid-mode -1))))

(use-package! vertico-buffer
  :after vertico
  :config
  (add-hook! 'vertico-buffer-mode-hook
    (defun vertico-buffer-h ()
      (vertico-posframe-mode (if vertico-buffer-mode -1 +1)))))

(after! vertico
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t)

  ;; Prefix current candidate with arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(after! embark
  (setq embark-cycle-key (kbd "C-;"))

  (defun +edebug-instrument-symbol (symbol)
    (interactive "sSymbol: ")
    (edebug-instrument-function (intern symbol)))

  (add-hook! 'embark-collect-mode-hook
    (defun visual-line-mode-off-h ()
      (visual-line-mode -1)))

  (defadvice! embark-prev-next-recenter-a ()
    :after #'embark-previous-symbol
    :after #'embark-next-symbol
    (recenter)))

(use-package! info+
  :commands (info info-display-manual)
  :config
  (setq Info-fontify-angle-bracketed-flag nil)
  (add-hook 'Info-mode-hook (lambda () (require 'info+))))

(after! smartparens
  ;; fix for smartparens. Doom's default module does things like skipping pairs if
  ;; one typed at the beginning of the word.
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             :unless '(sp-point-before-same-p)))

  (sp-pair "\"" nil :unless '(sp-point-before-word-p
                              sp-point-after-word-p))

  (sp-local-pair sp-lisp-modes "(" ")"
                 :wrap ")"
                 :unless '(:rem sp-point-before-same-p)))

(use-package! expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  (global-subword-mode +1)
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"
        expand-region-subword-enabled t))

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
    (buffer-local-value 'buffer-file-name buf)))

(after! avy
  (setq avy-all-windows t)
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark))

(after! consult
  (consult-customize +default/search-buffer :preview-key 'any))

;; ensure that browsing in Helpful and Info modes doesn't create additional window splits
(add-to-list
 'display-buffer-alist
 `(,(rx bos (or "*helpful" "*info"))
   (display-buffer-reuse-window
    display-buffer-in-direction)
   (direction . right)
   (window . root)
   (window-width . 0.35)))

(setq +doom-indent-sensitive-modes '())
(setq +doom-yank-indent-modes '())
(setq +doom-yank-indent-threshold 1000)

(defadvice! +yank-indent-region-a (yank-fn &rest args)
  :around #'yank
  :around #'yank-pop
  :around #'evil-paste-before
  :around #'evil-paste-after
  ;; borrowed from spacemacs altered for Doom. see spacemacs//yank-indent-region
  (evil-start-undo-step)
  (prog1
      (let ((prefix (car args))
            (enable (and (not (member major-mode +doom-indent-sensitive-modes))
                         (or (derived-mode-p 'prog-mode)
                             (member major-mode +doom-yank-indent-modes)))))
        (when (and enable (equal '(4) prefix))
          (setq args (cdr args)))
        (prog1
            (apply yank-fn args)
          (when (and enable (not (equal '(4) prefix)))
            (let ((transient-mark-mode nil)
                  (save-undo buffer-undo-list))
              (+yank-advised-indent-function (region-beginning)
                                             (region-end))))))
    (evil-end-undo-step)))

(after! edit-indirect
  ;; I want indirect buffers to always appear on the right side of current window
  (add-to-list
   'display-buffer-alist
   `("\\*edit-indirect .*\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right))))
