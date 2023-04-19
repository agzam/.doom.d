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
      (map! :leader :n wn f
            :n (concat "w" wn) f)
      (global-set-key (kbd k) f))))


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
   ibuffer-show-empty-filter-groups nil
   ibuffer-jump-offer-only-visible-buffers t)

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

  (define-ibuffer-filter non-special-buffers
      "Only show non-special buffers (without earmuffs)."
    (:description "non-special buffers")
    (ignore qualifier)
    (string-match "^[^*].*" (buffer-name buf))))

(after! avy
  (setq avy-all-windows t)
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark))


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

(after! evil
  (advice-add #'evil-ex-start-word-search :around #'evil-ex-visual-star-search-a))

(use-package! ibuffer-sidebar
  :defer t
  :commands (ibuffer-siderbar-toggle-sidebar)
  :config
  (add-hook!
   'ibuffer-sidebar-mode-hook
   (defun ibuffer-sidebar-h ()
     (ibuffer-vc-set-filter-groups-by-vc-root)
     (ibuffer-do-sort-by-recency)
     (call-interactively #'ibuffer-filter-by-non-special-buffers)))

  (setq ibuffer-sidebar-use-custom-font t
        ibuffer-sidebar-face `(:height 0.9)
        ibuffer-sidebar-width 20
        ibuffer-sidebar-pop-to-sidebar-on-toggle-open nil))
