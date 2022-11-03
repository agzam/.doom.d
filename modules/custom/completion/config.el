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
   corfu-auto-delay 0.0
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
  (when (modulep! +minibuffer)
    (add-hook 'minibuffer-setup-hook #'+corfu--enable-in-minibuffer))

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
  (after! evil
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map)
    (advice-add 'evil-escape-func :after 'corfu-quit))

  ;; TODO: check how to deal with Daemon/Client workflow with that
  (unless (display-graphic-p)
    (corfu-doc-terminal-mode)
    (corfu-terminal-mode)))


(use-package! corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :custom
  (corfu-doc-delay 0)
  :bind (:map corfu-map
              ("M-j" . corfu-doc-scroll-down)
              ("M-k" . corfu-doc-scroll-up)))


(use-package! orderless
  :init
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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-abbrev t))


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
  (add-hook 'doom-load-theme-hook #'kind-icon-reset-cache)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
