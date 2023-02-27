;;; custom/completion/config.el -*- lexical-binding: t; -*-

;;; A lot of it here borrowed from Takeshi Tsukamoto
;;; https://github.com/itome/.doom.d/

(use-package! corfu
  :hook
  (doom-first-buffer . global-corfu-mode)
  :config
  (setq
   corfu-separator ?\s
   corfu-auto t
   corfu-auto-delay 0.3
   corfu-preview-current nil ; Disable current candidate preview
   corfu-on-exact-match nil
   corfu-quit-no-match 'separator
   corfu-cycle t
   corfu-auto-prefix 2
   completion-cycle-threshold 1
   tab-always-indent 'complete
   corfu-max-width 56
   corfu-min-width 56
   corfu-preselect-first t)
  ;; Not sure if completions in the minibuffer helpful or annoying. may enable it again later
  ;; (when (modulep! +minibuffer)
  ;;   (add-hook 'minibuffer-setup-hook #'+corfu--enable-in-minibuffer))

  (add-hook! 'doom-init-modules-hook
    (defun reset-lsp-completion-provider-h ()
      (after! lsp-mode
        (setq lsp-completion-provider :none))))

  (add-hook! 'lsp-completion-mode-hook
    (defun init-orderless-lsp-completions-h ()
      (setf (alist-get 'lsp-capf completion-category-defaults)
            '((styles . (orderless flex))))))

  (map! :map corfu-map
        "<escape>" #'evil-collection-corfu-quit-and-escape
        "C-SPC"    #'corfu-insert-separator
        "C-n"      #'corfu-next
        "C-p"      #'corfu-previous
        "C-/" #'+corfu-move-to-minibuffer
        (:prefix ("C-x" . "cape")
                 "C-k"     #'cape-dict
                 "s"       #'cape-ispell
                 "C-n"     #'cape-keyword
                 "C-f"     #'cape-file))
  ;; corfu-indexed to work just like in company, press a C+number - inserts the thing
  (map! :map corfu-map
        "C-0" (cmd! () (+corfu-insert-indexed 9))
        "C-1" (cmd! () (+corfu-insert-indexed 0))
        "C-2" (cmd! () (+corfu-insert-indexed 1))
        "C-3" (cmd! () (+corfu-insert-indexed 2))
        "C-4" (cmd! () (+corfu-insert-indexed 3))
        "C-5" (cmd! () (+corfu-insert-indexed 4))
        "C-6" (cmd! () (+corfu-insert-indexed 5))
        "C-7" (cmd! () (+corfu-insert-indexed 6))
        "C-8" (cmd! () (+corfu-insert-indexed 7))
        "C-9" (cmd! () (+corfu-insert-indexed 8)))

  (after! evil
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map)
    (advice-add 'evil-escape-func :after 'corfu-quit))

  ;; TODO: check how to deal with Daemon/Client workflow with that
  (unless (display-graphic-p)
    (corfu-doc-terminal-mode)
    (corfu-terminal-mode)))


(use-package! orderless
  :config
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(use-package! cape
  :defer t
  :init
  (map!
   [remap dabbrev-expand] 'cape-dabbrev)
  (add-hook! 'latex-mode-hook
    (defun +corfu--latex-set-capfs ()
      (add-to-list 'completion-at-point-functions #'cape-tex)))

  (when (modulep! :custom writing)
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-ispell))
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))


(use-package! corfu-popupinfo
  :after corfu
  :config
  (setq corfu-popupinfo-delay t)
  (corfu-popupinfo-mode +1))


(use-package! corfu-history
  :after corfu
  :config
  (add-hook! corfu-mode
    (defun corfu-mode-history-h ()
      (corfu-history-mode 1)
      (savehist-mode 1)
      (add-to-list 'savehist-additional-variables 'corfu-history))))


(use-package! corfu-indexed
  :after corfu
  :config
  (setq corfu-indexed-start 1)
  (add-hook! corfu-mode #'corfu-indexed-mode))


(use-package! corfu-quick
  :after corfu
  :bind (:map corfu-map
              ("M-q" . corfu-quick-complete)
              ("C-q" . corfu-quick-insert)))


(use-package! kind-icon
  :after corfu
  :when (modulep! +icons)
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (setq kind-icon-use-icons t
        svg-lib-icons-dir (expand-file-name "svg-lib" doom-cache-dir)
        kind-icon-mapping
        '((array "a" :icon "code-brackets" :face font-lock-variable-name-face)
          (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
          (class "c" :icon "view-grid-plus-outline" :face font-lock-type-face)
          (color "#" :icon "palette" :face success)
          (constant "co" :icon "pause-circle" :face font-lock-constant-face)
          (constructor "cn" :icon "table-column-plus-after" :face font-lock-function-name-face)
          (enum "e" :icon "format-list-bulleted-square" :face font-lock-builtin-face)
          (enum-member "em" :icon "format-list-checks" :face font-lock-builtin-face)
          (event "ev" :icon "lightning-bolt-outline" :face font-lock-warning-face)
          (field "fd" :icon "application-braces-outline" :face font-lock-variable-name-face)
          (file "f" :icon "file" :face font-lock-string-face)
          (folder "d" :icon "folder" :face font-lock-doc-face)
          (function "f" :icon "sigma" :face font-lock-function-name-face)
          (interface "if" :icon "video-input-component" :face font-lock-type-face)
          (keyword "kw" :icon "image-filter-center-focus" :face font-lock-keyword-face)
          (macro "mc" :icon "lambda" :face font-lock-keyword-face)
          (method "m" :icon "sigma" :face font-lock-function-name-face)
          (module "{" :icon "view-module" :face font-lock-preprocessor-face)
          (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
          (operator "op" :icon "plus-circle-outline" :face font-lock-comment-delimiter-face)
          (param "pa" :icon "cog" :face default)
          (property "pr" :icon "tune-vertical" :face font-lock-variable-name-face)
          (reference "rf" :icon "bookmark-box-multiple" :face font-lock-variable-name-face)
          (snippet "S" :icon "text-short" :face font-lock-string-face)
          (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
          (struct "%" :icon "code-braces" :face font-lock-variable-name-face)
          (t "." :icon "crosshairs-question" :face shadow)
          (text "tx" :icon "script-text-outline" :face shadow)
          (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
          (unit "u" :icon "ruler-square" :face shadow)
          (value "v" :icon "numeric-1-box-multiple-outline" :face font-lock-builtin-face)
          (variable "va" :icon "adjust" :face font-lock-variable-name-face)))
  (setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.8))
  (add-hook 'doom-load-theme-hook #'kind-icon-reset-cache)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


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
   vertico-posframe-height 17
   vertico-count 15
   vertico-posframe-width 120
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
    (defun restore-vertico-posframe-after-term-h (_frame)
      (when (bound-and-true-p vertico-posframe-restore-after-term-p)
        (vertico-posframe-mode +1))))

  ;; fixing "Doesn't properly respond to C-n"
  ;; https://github.com/tumashu/vertico-posframe/issues/11
  (defadvice! vertico-posframe--display-no-evil (fn lines)
    :around #'vertico-posframe--display
    (funcall-interactively fn lines)
    (evil-local-mode -1))

  (map! :after vertico
        :map vertico-map
        "C-c C-p"  #'vertico-posframe-briefly-off
        "C-." #'vertico-posframe-briefly-transparent))

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
                 cand)))

  (map! :map vertico-map
        "C-'" #'vertico-quick-insert
        "C-h" #'vertico-directory-delete-word
        "C-c C-g" #'vertico-grid-mode
        "M-h" #'vertico-grid-left
        "M-l" #'vertico-grid-right
        "M-j" #'vertico-next
        "M-k" #'vertico-previous
        "C-e" #'vertico-scroll-up
        "C-y" #'vertico-scroll-down
        "]" #'vertico-next-group
        "[" #'vertico-previous-group
        "~" #'vertico-jump-to-home-dir-on~))

(after! consult
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")

  (setq consult-preview-key "C-SPC")
  (consult-customize
   +default/search-buffer
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  (define-key!
    :keymaps (append +default-minibuffer-maps)
    "C-/" #'consult-history)

  (map! :after consult
        :map isearch-mode-map "M-s l" #'consult-line))

(after! embark
  (setq embark-cycle-key "C-;")

  (map!
   :after embark
   (:map
    embark-file-map
    "o" nil
    (:prefix ("o" . "open")
             "j" (embark-split-action find-file +evil/window-split-and-follow)
             "l" (embark-split-action find-file +evil/window-vsplit-and-follow)
             "h" (embark-split-action find-file split-window-horizontally)
             "k" (embark-split-action find-file split-window-vertically)
             "a" (embark-ace-action find-file)))

   (:map
    embark-buffer-map
    "o" nil
    (:prefix ("o" . "open")
             "j" (embark-split-action switch-to-buffer +evil/window-split-and-follow)
             "a" (embark-ace-action switch-to-buffer)))

   (:map
    embark-function-map
    "o" nil
    (:prefix ("d" . "definition")
             "j" (embark-split-action xref-find-definitions +evil/window-split-and-follow)
             "l" (embark-split-action xref-find-definitions +evil/window-vsplit-and-follow)
             "h" (embark-split-action xref-find-definitions split-window-horizontally)
             "k" (embark-split-action xref-find-definitions split-window-vertically)
             "a" (embark-ace-action xref-find-definitions)))

   (:map
    embark-url-map
    "e" #'+eww-open-in-other-window
    "b" #'+browse-url)

   (:map
    embark-collect-mode-map
    :n "[" #'embark-previous-symbol
    :n "]" #'embark-next-symbol)

   (:map
    (embark-command-map embark-symbol-map)
    (:after edebug
            (:prefix ("D" . "debug")
                     "f" #'+edebug-instrument-symbol
                     "F" #'edebug-remove-instrumentation))))

  (add-hook! 'embark-collect-mode-hook
    (defun visual-line-mode-off-h ()
      (visual-line-mode -1)))

  (defadvice! embark-prev-next-recenter-a ()
    :after #'embark-previous-symbol
    :after #'embark-next-symbol
    (recenter)))
