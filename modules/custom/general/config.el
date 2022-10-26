;;; common/general/config.el -*- lexical-binding: t; -*-

;; disable nonsensical keys
(dolist (key '("s-n" "s-p" "s-q" "s-m" "C-x C-c"))
  (global-set-key (kbd key) nil))

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp
      :v "s" #'evil-surround-region
      "s-b" #'consult-buffer
      "s-=" #'text-scale-increase
      "s--" #'text-scale-decrease
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter))
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter))
      :n "zk" #'text-scale-increase
      :n "zj" #'text-scale-decrease
      :n "s-e" #'+scroll-line-down-other-window
      :n "s-y" #'+scroll-line-up-other-window)

(map! :map minibuffer-mode-map
      "M-l" #'sp-forward-slurp-sexp
      "M-h" #'sp-forward-barf-sexp)

(put 'narrow-to-region 'disabled nil)

(map! :leader
      "TAB"   #'alternate-buffer
      "v"     #'er/expand-region
      ";"     #'evilnc-comment-or-uncomment-lines
      "C-h k" #'helpful-key

      ;; liberate the top level bindings for other things
      ;; "a" nil
      "x" nil

      (:prefix ("b" . "buffers")
       :desc "scratch" "s" #'doom/switch-to-scratch-buffer
       :desc "Messages" "m" #'switch-to-messages-buffer
       "d"   #'kill-this-buffer
       "k"   #'kill-buffer-and-window
       "D"   #'diff-current-buffer-with-file
       "s-d" #'spacemacs/kill-matching-buffers-rudely)

      (:prefix ("f" . "files")
       :desc "fasd dir" "ad" (cmd! (fasd-find-file 1))
       :desc "fasd file" "af" (cmd! (fasd-find-file -1))
       "e" nil ;; release it, or it complains
       (:prefix ("e" . "doom/emacs")
        :desc "doom.d" "d" #'find-in-doom-dir
        :desc "doom init dir" "i" (cmd! () (dired doom-emacs-dir))))

      (:prefix ("g" . "goto")
               "f" #'magit-file-dispatch
               "j" #'evil-show-jumps
               "s" #'magit-status)

      (:prefix ("h" . "help")
               "a" #'helpful-at-point
               "dd" nil ; muscle memory is still strong
               "f" #'helpful-function
               "h" #'helpful-symbol
               "p" nil
               (:prefix ("p" . "packages")
                        "l" #'list-packages
                        "f" #'find-library-other-window
                        "d" #'doom/describe-package)
               "s" #'find-function-other-window
               "v" #'helpful-variable)

      (:prefix ("j" . "jump")
               "j" #'avy-goto-char-timer
               "x" #'xwidget-webkit-url-get-create)

      (:prefix ("k" .  "lispy")
               "=" #'sp-reindent
               "-" #'sp-reindent
               "W" #'sp-unwrap-sexp
               "b" #'sp-forward-barf-sexp
               "B" #'sp-backward-barf-sexp
               "c" #'sp-convolute-sexp
               "dx" #'sp-kill-sexp
               "r" #'sp-raise-sexp
               "s" #'sp-forward-slurp-sexp
               "S" #'sp-backward-slurp-sexp
               "t" #'sp-transpose-sexp
               "w" #'sp-wrap-sexp
               "y" #'sp-copy-sexp)

      (:prefix ("n" . "narrow")
               "F" #'narrow-to-defun-indirect-buffer
               "R" #'narrow-to-region-indirect-buffer
               "f" #'narrow-to-defun
               "r" #'narrow-to-region
               "l" #'consult-focus-lines
               "w" (cmd! (consult-focus-lines :show) (widen)))

      (:prefix ("o" . "open/Org")
       :desc "store link"      "l" #'org-store-link
       :desc "link without id" "L" #'org-store-link-id-optional
       (:when (modulep! :custom notmuch) :desc "notmuch" "m" #'notmuch)
       (:prefix ("g" . "git")
                "h" #'gh-notify))

      (:prefix ("p" . "projects")
       :desc "Invalidate project cache" "I" #'projectile-invalidate-cache
       :desc "project IBuffer" "i" #'projectile-ibuffer)

      (:prefix ("r" . "resume/ring")
               "y" #'consult-yank-from-kill-ring)

      (:prefix ("s". "search/symbol")
               "/" #'engine/search-google
               "e" #'eww-search-words
               "f" #'find-name-dired
               "g" #'engine/search-github-with-lang
               "j" #'imenu)

      (:prefix ("t" . "toggle")
               "w" #'toggle-visual-line-navigation)

      (:prefix ("w" . "windows")
               "." #'window-transient
               "c" #'window-cleanup+
               "g" #'golden-ratio
               "D" #'ace-delete-window
               "M" #'ace-swap-window
               "W" #'ace-window
               "_" #'delete-other-windows-horizontally
               "m" #'toggle-maximize-buffer
               "|" #'delete-other-windows-vertically
               "r" #'balance-windows-area
               "=" #'balance-windows-area)

      (:prefix ("x" ."text")
               "b" #'flyspell-correct-previous
               "x" #'flyspell-correct-at-point)

      (:prefix ("z" . "zoom")
               "f" #'frame-zoom-transient))

(map! :map special-mode-map
      "SPC" nil
      "h" #'evil-backward-char)

;; Change the cursor color in emacs state. We do it this roundabout way
;; to ensure changes in theme doesn't break these colors.
(add-hook! '(doom-load-theme-hook doom-init-modules-hook)
  (defun +evil-update-cursor-color-h ()
    (put 'cursor 'evil-emacs-color "SkyBlue2")
    (put 'cursor 'evil-normal-color "DarkGoldenrod2")
    (posframe-delete-all)))

(after! evil-maps
  ;; often conflicts with doom-local-leader
  ;; (unbind-key (kbd ",") evil-motion-state-map)
  (map! (:map evil-motion-state-map "C-u" nil)
        (:map evil-insert-state-map "C-u" nil)))

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
  (setq
   vertico-posframe-global t
   vertico-posframe-height 23
   vertico-posframe-width 200
   marginalia-margin-threshold 500)
  (vertico-posframe-mode +1)
  (map! :map vertico-map "C-c C-p"  #'vertico-posframe-briefly-off)

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
  (map! :leader "rl" #'vertico-repeat-last)
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

(use-package! vertico-buffer
  :after vertico
  :config
  (add-hook! 'vertico-buffer-mode-hook
    (defun vertico-buffer-h ()
      (vertico-posframe-mode (if vertico-buffer-mode -1 +1)))))

(after! vertico
  (map! :map vertico-map
        "C-e" #'vertico-scroll-up
        "C-y" #'vertico-scroll-down
        "]" #'vertico-next-group
        "[" #'vertico-previous-group)

  (map! :map minibuffer-local-map "C-c C-s" #'embark-collect)

  (map! :map vertico-map
        "~" #'vertico-jump-to-home-dir-on~)

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
  (map! :map embark-file-map
        "o" nil
        (:prefix ("o" . "open")
                 "j" (embark-split-action find-file +evil/window-split-and-follow)
                 "l" (embark-split-action find-file +evil/window-vsplit-and-follow)
                 "h" (embark-split-action find-file split-window-horizontally)
                 "k" (embark-split-action find-file split-window-vertically)
                 "a" (embark-ace-action find-file)))
  (map! :map embark-buffer-map
        "o" nil
        (:prefix ("o" . "open")
                 "j" (embark-split-action switch-to-buffer +evil/window-split-and-follow)
                 "a" (embark-ace-action switch-to-buffer)))
  (map! :map embark-function-map
        "o" nil
        (:prefix ("d" . "definition")
                 "j" (embark-split-action xref-find-definitions +evil/window-split-and-follow)
                 "l" (embark-split-action xref-find-definitions +evil/window-vsplit-and-follow)
                 "h" (embark-split-action xref-find-definitions split-window-horizontally)
                 "k" (embark-split-action xref-find-definitions split-window-vertically)
                 "a" (embark-ace-action xref-find-definitions)))

  (defun +edebug-instrument-symbol (symbol)
    (interactive "sSymbol: ")
    (edebug-instrument-function (intern symbol)))

  (map! :map (embark-command-map embark-symbol-map)
        (:after edebug
                (:prefix ("D" . "debug")
                         "f" #'+edebug-instrument-symbol
                         "F" #'edebug-remove-instrumentation)))

  (add-hook! 'embark-collect-mode-hook
    (defun visual-line-mode-off-h ()
      (visual-line-mode -1)))

  (map! :map embark-collect-mode-map
        :n "[" #'embark-previous-symbol
        :n "]" #'embark-next-symbol)

  (defadvice! embark-prev-next-recenter-a ()
    :after #'embark-previous-symbol
    :after #'embark-next-symbol
    (recenter)))

(use-package! info+
  :commands (info info-display-manual)
  :init
  (map! :leader "hj" #'info-display-manual)
  :config
  (setq Info-fontify-angle-bracketed-flag nil)
  (add-hook 'Info-mode-hook (lambda () (require 'info+))))

(use-package! company-posframe
  :after company
  :hook (company-mode . company-posframe-mode)
  :init
  (setq company-posframe-quickhelp-delay nil
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
    (buffer-local-value 'buffer-file-name buf))

  (map! :map ibuffer-mode-map
        :n "su" #'ibuffer-filter-by-unsaved-file-buffers
        :n "sF" #'ibuffer-filter-by-file-buffers))

(after! avy
  (setq avy-all-windows t)
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark))

(map! :map occur-mode-map
      :n "f" #'occur-mode-display-occurrence)

(after! consult
  (consult-customize +default/search-buffer :preview-key 'any)
  (map! :map isearch-mode-map "M-s l" #'consult-line))

(after! transient
  (map! :map transient-map "q" #'transient-quit-one
        :map transient-map "<escape>" #'transient-quit-one
        :map transient-edit-map "q" #'transient-quit-one
        :map transient-sticky-map "q" #'transient-quit-seq))

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

