;;; common/general/config.el -*- lexical-binding: t; -*-

;; disable nonsensical keys
(dolist (key '("s-n" "s-p" "s-q" "s-m" "C-x C-c"))
  (unbind-key (kbd key)))

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp
      :v "s" #'evil-surround-region
      "s-b" #'consult-buffer
      "s-=" #'text-scale-increase
      "s--" #'text-scale-decrease)

(put 'narrow-to-region 'disabled nil)

(map! :leader
      "TAB" #'alternate-buffer
      "v" #'er/expand-region
      ";" #'evilnc-comment-or-uncomment-lines
      "C-h k" #'helpful-key

      ;; liberate the top level bindings for other things
      "a" nil "x" nil

      "nF" #'narrow-to-defun-indirect-buffer
      "nR" #'narrow-to-region-indirect-buffer
      "nf" #'narrow-to-defun
      "nr" #'narrow-to-region
      "nw" #'widen
      (:prefix ("z" . "zoom")
       "f" #'+hydra/text-zoom/body)
      (:prefix ("w" . "windows")
       "D" #'ace-delete-window
       "M" #'ace-swap-window
       "W" #'ace-window
       "_" #'delete-other-windows-horizontally
       "m" #'toggle-maximize-buffer
       "|" #'delete-other-windows-vertically
       "=" #'balance-windows-area)
      (:prefix ("k" .  "lispy")
       "=" #'sp-reindent
       "W" #'sp-unwrap-sexp
       "b" #'sp-forward-barf-sexp
       "dx" #'sp-kill-sexp
       "r" #'sp-raise-sexp
       "s" #'sp-forward-slurp-sexp
       "t" #'sp-transpose-sexp
       "w" #'sp-wrap-sexp
       "y" #'sp-copy-sexp)
      (:prefix ("a" . "apps/actions")
       "a" #'embark-act
       (:prefix "g"
        "h" #'gh-notify))
      (:prefix ("b" . "buffers")
       :desc "scratch" "s" #'doom/switch-to-scratch-buffer
       :desc "Messages" "m" #'switch-to-messages-buffer
       "d" #'kill-this-buffer
       "s-d" #'spacemacs/kill-matching-buffers-rudely)
      (:prefix ("e" . "emacs/doom")
       "d" #'doom/goto-private-config-file
       "i" (lambda () (interactive) (dired doom-emacs-dir)))
      (:prefix ("f" . "files")
       "ad" #'fasd-find-file
       "e" nil ;; release it, or it complains
       (:prefix ("e" . "doom/emacs")
        "d" #'doom/goto-private-config-file
        :desc "doom init dir" "i" (lambda () (interactive) (dired doom-emacs-dir))))
      (:prefix ("g" . "goto")
       "f" #'magit-file-dispatch
       "j" #'evil-show-jumps
       "s" #'magit-status)
      (:prefix ("h" . "help")
       "a" #'helpful-at-point
       "f" #'helpful-function
       "h" #'helpful-symbol
       "v" #'helpful-variable
       ;; muscle memory is still strong
       "dd" nil)
      (:prefix ("j" . "jump")
       "j" #'avy-goto-char-timer
       "x" #'xwidget-webkit-url-get-create)
      (:prefix ("r" . "resume/ring")
       "y" #'yank-from-kill-ring)
      (:prefix ("s". "search/symbol")
       "j" #'imenu
       "/" #'engine/search-google
       "g" #'engine/search-github-with-lang)
      (:prefix ("t" . "toggle")
       "w" #'toggle-visual-line-navigation)
      (:prefix ("x" ."text")
       "b" #'flyspell-correct-previous
       "x" #'flyspell-correct-at-point))

(map! :localleader :map xwidget-webkit-mode-map "x" #'kill-current-buffer)


;; Change the cursor color in emacs state. We do it this roundabout way
;; to ensure changes in theme doesn't break these colors.
;; (add-hook! '(doom-load-theme-hook doom-init-modules-hook)
;;   (defun +evil-update-cursor-color-h ()
;;     (put 'cursor 'evil-emacs-color "SkyBlue2")
;;     (put 'cursor 'evil-normal-color "DarkGoldenrod2")))

(after! evil-maps
  ;; often conflicts with doom-local-leader
  ;; (unbind-key (kbd ",") evil-motion-state-map)
  (unbind-key (kbd "C-u") evil-motion-state-map))

(use-package winum
  :after-call doom-switch-window-hook
  :config
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

(use-package! consult-company
  :after company consult
  :config
  (map! :map company [remap completion-at-point] #'consult-company))

;;;;;;;;;;;;;;;;;;;
;; vertico stuff ;;
;;;;;;;;;;;;;;;;;;;

;; Add vertico extensions load path
(add-to-list 'load-path (format "%sstraight/build-%s/vertico/extensions/" (file-truename doom-local-dir) emacs-version))

(use-package! vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-mode +1)
  (setq vertico-posframe-global t
        vertico-posframe-height 22
        vertico-posframe-width 200
        ;; marginalia-margin-threshold 300
        )

  (add-hook! 'minibuffer-exit-hook #'restore-vertico-posframe-state-h)
  (map! :map vertico-map "C-c C-p"  #'vertico-posframe-temporarily-off))

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
        "C-c C-g" #'vertico-grid-mode
        "M-h" #'vertico-grid-left
        "M-l" #'vertico-grid-right
        "M-j" #'vertico-next
        "M-k" #'vertico-previous)
  (add-hook! 'minibuffer-exit-hook
    (defun vertico-grid-mode-off ()
      (vertico-grid-mode -1))))

(use-package! vertico-buffer :after vertico)

(after! vertico
 (map! :map vertico-map
       "C-u" nil  ; unbind universal argument
       "C-e"      #'vertico-scroll-up
       "C-y"      #'vertico-scroll-down))

(use-package! info+
  :after info
  :config
  (map! :leader "hj" #'info-display-manual)
  (setq Info-fontify-angle-bracketed-flag nil)
  (add-hook 'Info-mode-hook (lambda () (require 'info+))))

(use-package! company-posframe
  :after company
  :hook (company-mode . company-posframe-mode)
  :init
  (setq company-posframe-quickhelp-delay 1
        company-posframe-show-indicator nil
        company-quickhelp-delay nil)
  :bind (:map company-active-map
         ("C-h" . (lambda () (interactive) (company-posframe-quickhelp-show)))
         ("C-c C-d". company-show-doc-buffer)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-c C-l". company-show-location)
         :map company-posframe-active-map
         ("C-c h" . company-posframe-quickhelp-toggle)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (company-posframe-mode +1)
  ;; doom-modeline keeps re-rendendering through company completion changes
  ;; (add-hook! 'company-completion-started-hook
  ;;   (defun doom-modeline-off (_)
  ;;     (doom-modeline-mode -1)))
  ;; (add-hook! ('company-completion-finished-hook
  ;;             'company-completion-cancelled-hook)
  ;;            #'doom-modeline-mode)
  )

;; (use-package! unicode-fonts
;;   :after-call doom-init-ui-h
;;   :init
;;   (when (and unicode-fonts-force-multi-color-on-mac
;;              (eq window-system 'ns))
;;     (setq unicode-fonts-skip-font-groups '(decorative low-quality-glyphs)))
;;   (unicode-fonts-setup))


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

(after! expand-region
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

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

;; ensure that browsing in Helpful and Info modes doesn't create additional window splits
(add-to-list
 'display-buffer-alist
 `(,(rx bos (or "*helpful" "*info"))
   (display-buffer-reuse-window
    display-buffer-in-direction)
   (direction . right)
   (window . root)
   (window-width . 0.3)))
