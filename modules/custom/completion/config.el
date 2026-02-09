;;; custom/completion/config.el -*- lexical-binding: t; -*-

;;; A lot of it here borrowed from Takeshi Tsukamoto
;;; https://github.com/itome/.doom.d/

(use-package! corfu
  :defer t
  :hook
  (doom-first-buffer . global-corfu-mode)
  :config
  (setq
   corfu-separator ?\s
   corfu-auto t
   corfu-auto-delay 0.5
   corfu-preview-current nil ; Disable current candidate preview
   corfu-on-exact-match 'insert
   corfu-preview-current 'insert
   corfu-quit-no-match 'separator
   corfu-cycle t
   corfu-auto-prefix 3
   completion-cycle-threshold 1
   tab-always-indent 'complete
   corfu-count 9)
  ;; (when (modulep! +minibuffer)
  ;;   (add-hook 'minibuffer-setup-hook #'+corfu--enable-in-minibuffer))

  (add-hook! 'doom-init-modules-hook
    (defun reset-lsp-completion-provider-h ()
      (after! lsp-mode
        (setq lsp-completion-provider :none))))

  (defun lsp-completion-off-in-text-modes-h ()
    (when (member major-mode '(text-mode org-mode markdown-mode
                               message-mode git-commit-mode))
      (lsp-completion-mode -1)))

  (add-hook! 'lsp-completion-mode-hook
    (defun init-orderless-lsp-completions-h ()
      (setf (alist-get 'lsp-capf completion-category-defaults)
            '((styles . (orderless flex)))))
    (defun lsp-completion-off-in-text-modes-h ()
      (when (and lsp-completion-mode
                 (member major-mode '(text-mode org-mode markdown-mode
                                      message-mode git-commit-mode)))
        (lsp-completion-mode -1))))

  (map! :map corfu-map
        "<escape>" #'+corfu-quit-and-escape
        "C-SPC"    #'corfu-insert-separator
        "C-n"      #'corfu-next
        "C-p"      #'corfu-previous
        "C-/" #'+corfu-move-to-minibuffer
        :i "C-u" nil ; evil-collection bs
        (:prefix ("C-c p" . "cape")
                 "p"  #'complete-tag
                 "t"  #'cape-dabbrev
                 "d"  #'cape-history
                 "h"  #'cape-file
                 "f"  #'cape-keyword
                 "k"  #'cape-symbol
                 "s"  #'cape-abbrev
                 "a"  #'cape-line
                 "l"  #'cape-dict
                 "w"  #'cape-tex
                 "_"  #'cape-tex
                 "&"  #'cape-sgml
                 "r"  #'cape-rfc1345
                 "y"  #'yasnippet-capf))
  ;; corfu-indexed like in Company, M+number - inserts the thing
  (map! :map corfu-map
        "M-0" (cmd! () (+corfu-insert-indexed 9))
        "M-1" (cmd! () (+corfu-insert-indexed 0))
        "M-2" (cmd! () (+corfu-insert-indexed 1))
        "M-3" (cmd! () (+corfu-insert-indexed 2))
        "M-4" (cmd! () (+corfu-insert-indexed 3))
        "M-5" (cmd! () (+corfu-insert-indexed 4))
        "M-6" (cmd! () (+corfu-insert-indexed 5))
        "M-7" (cmd! () (+corfu-insert-indexed 6))
        "M-8" (cmd! () (+corfu-insert-indexed 7))
        "M-9" (cmd! () (+corfu-insert-indexed 8)))

  (after! evil
    ;; (defadvice! evil-normalize-keymaps-corfu-setup-a (_beg _end _table _pred)
    ;;   :after 'corfu--setup
    ;;   (evil-normalize-keymaps))
    ;; (defadvice! evil-normalize-keymaps-corfu-teardown-a (&rest _)
    ;;   :after 'corfu--teardown
    ;;   (evil-normalize-keymaps))
    (evil-make-overriding-map corfu-map)
    (advice-add 'evil-escape-func :after 'corfu-quit))

  (setq dabbrev-ignored-buffer-modes '(pdf-view-mode dired-mode vterm-mode)))

(use-package! orderless
  :after-call doom-first-input-hook
  :config
;;   (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
;;     "Highlight company matches correctly, and try default completion styles before
;; orderless."
;;     :around #'company-capf--candidates
;;     (let ((orderless-match-faces [completions-common-part])
;;           (completion-styles +vertico-company-completion-styles))
;;       (apply fn args)))

  (setopt orderless-affix-dispatch-alist
          '((?! . orderless-without-literal)
            (?& . orderless-annotation)
            (?% . char-fold-to-regexp)
            (?` . orderless-initialism)
            (?= . orderless-literal)
            (?^ . orderless-literal-prefix)
            (?~ . orderless-flex))
          orderless-style-dispatchers
          '(+vertico-orderless-dispatch
            +vertico-orderless-disambiguation-dispatch))

  (setopt completion-styles '(orderless partial-completion basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion)))
                                          (symbol (styles . (partial-completion))))))

(use-package! corfu-terminal
  :defer t
  :when (not (display-graphic-p))
  :hook ((corfu-mode . corfu-terminal-mode)))

(use-package! cape
  :after corfu
  :init
  (map! [remap dabbrev-expand] 'cape-dabbrev)
  (add-hook! latex-mode
    (defun +corfu--latex-set-capfs ()
      (add-to-list 'completion-at-point-functions #'cape-tex)))

  (add-hook! (text-mode prog-mode)
    (defun cape-completion-at-point-functions-h ()
      (dolist (cfn '(yasnippet-capf
                     cape-dabbrev
                     cape-abbrev
                     cape-dict
                     cape-file
                     cape-keyword))
        (add-to-list 'completion-at-point-functions cfn :append)
        (setq-local completion-at-point-functions
              (remove 'ispell-completion-at-point
                      completion-at-point-functions)))))

  (add-hook! emacs-lisp-mode
    (defun +cape-completion-at-point-elisp-h ()
      (add-to-list 'completion-at-point-functions #'cape-elisp-symbol :append)))

  (add-hook! (org-mode markdown-mode)
    (defun +cape-completion-at-point-org-md-h ()
      (add-to-list 'completion-at-point-functions #'cape-elisp-block :append)))

  ;; (add-hook! '(eshell-mode-hook comint-mode-hook minibuffer-setup-hook)
  ;;   (defun +cape-completion-at-point-history-h ()
  ;;     (add-to-list 'completion-at-point-functions #'cape-history)))
  )

(use-package! corfu-popupinfo
  :after corfu
  :config
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
              ("C-q" . corfu-quick-insert))
  :config (setq corfu-quick1 "asdfghjkl;"))

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

(use-package! vertico
  :hook (doom-first-input . vertico-mode)
  :init
  (defadvice! +vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map "DEL" #'vertico-directory-delete-char)

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args)))
  (setq completion-ignore-case t
        read-buffer-completion-ignore-case t)

  (defadvice! vertico-current-with-arrow-a
    ;; Prefix current candidate with arrow
    (orig cand prefix suffix index _start)
    :around #'vertico--format-candidate
    (setq cand (funcall orig cand prefix suffix index _start))
    (concat
     (if (= vertico--index index)
         (propertize "» " 'face 'vertico-current)
       "  ")
     cand))

  (map! :map vertico-map
        (:prefix ";"
         "." #'evil-insert-state
         "i" #'vertico-quick-insert
         "j" #'vertico-quick-jump
         "g" #'vertico-multiform-grid
         "b" #'vertico-multiform-buffer
         "f" #'vertico-multiform-flat
         "u" #'vertico-multiform-unobtrusive
         "r" #'vertico-multiform-reverse
         "t" #'vertico-posframe-briefly-tall
         ";" #'vertico-posframe-briefly-tall
         "p" #'vertico-multiform-posframe
         "s" #'embark-collect
         "e" #'embark-export
         "C-;" #'embark-act
         :desc "insert ;" "SPC" (cmd! (insert ";")))
        "DEL" #'delete-backward-char
        "C-h" #'vertico-directory-delete-word
        "M-h" #'vertico-grid-left
        "M-l" #'vertico-grid-right
        "M-j" #'vertico-next
        "M-k" #'vertico-previous
        "C-e" #'vertico-scroll-up
        "C-y" #'vertico-scroll-down
        "]" #'vertico-next-group
        "[" #'vertico-previous-group
        "~" #'vertico-jump-to-home-dir-on~
        "C-/" #'vertico-jump-root
        "C-?" #'vertico-jump-sudo
        "M-m" #'embark-select
        "C-SPC" #'embark-preview+))

;; Add vertico extensions load path
(add-to-list 'load-path (format "%sstraight/build-%s/vertico/extensions/" (file-truename doom-local-dir) emacs-version))

(use-package! vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (setq
   vertico-posframe-global t
   vertico-posframe-height nil
   ;; vertico-count 15
   vertico-posframe-width 150
   marginalia-margin-threshold 500
   vertico-posframe-parameters `((alpha . 1.0))
   ;; Ignore buffer-local text-scale and use frame's default font size
   posframe-text-scale-factor-function (lambda (_) 0))
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
    (evil-local-mode -1)))

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

(use-package consult
  :defer t
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    ;; [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)

  :config

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd
   :preview-key 'any)

  (setq consult-preview-key "C-SPC")
  (consult-customize
   +default/search-buffer
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  (define-key!
    :keymaps (append +default-minibuffer-maps)
    "C-/" #'consult-history)

  (map! :after consult
        :map isearch-mode-map "M-s l" #'consult-line)

  (remove-hook! 'consult-after-jump-hook 'consult--maybe-recenter)
  (add-hook! 'consult-after-jump-hook 'recenter))


(use-package! consult-dir
  :defer t
  :init
  (map! [remap list-directory] #'consult-dir
        (:after vertico
         :map vertico-map
         "C-x C-d" #'consult-dir
         "C-x C-j" #'consult-dir-jump-file))
  :config
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)

  ;; (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  ;; (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
  )

(use-package! consult-flycheck
  :after (consult flycheck))

(use-package! marginalia
  :hook (doom-first-input . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (when (modulep! +icons)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
  (advice-add #'marginalia--project-root :override #'doom-project-root)
  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
            '(doom/find-file-in-emacsd . project-file)
            '(doom/find-file-in-other-project . project-file)
            '(doom/find-file-in-private-config . file)
            '(doom/describe-active-minor-mode . minor-mode)
            '(flycheck-error-list-set-filter . builtin)
            '(persp-switch-to-buffer . buffer)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))

(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setopt wgrep-auto-save-buffer t))

(use-package! yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :config
  (map! :map yas-minor-mode-map
        "M-j" #'yas-next-field
        "M-k" #'yas-prev-field)
  (add-to-list 'yas-snippet-dirs (concat doom-user-dir "snippets/"))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all)
  (yas-global-mode +1)

  (add-hook! 'yas-before-expand-snippet-hook
             #'temporarily-disable-smart-parens
             #'evil-insert-state)
  (advice-add 'yas-completing-prompt :around #'yas-completing-prompt-a))

(use-package yasnippet-capf
  :after cape
  :config
  (add-hook! 'yas-minor-mode-hook :append
    (defun +corfu-remove-t-in-completion-at-point-functions ()
      (remove-hook! 'completion-at-point-functions :local 't))))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :config
  (setq consult-yasnippet-use-thing-at-point t))

(use-package! dash-docs
  :defer t
  :config
  (setq dash-docs-browser-func #'+browse-dash-doc
        dash-docs-enable-debugging nil)

  ;; a check, before activation of a docset to install it if needed
  (advice-add 'dash-docs-activate-docset :around #'dash-docs-activate-docset-a)

  ;;; overriding internal implementation fns for the time being
  ;;; https://github.com/dash-docs-el/dash-docs/issues/23
  (advice-add 'dash-docs-install-user-docset :override #'dash-docs-install-user-docset+)
  (advice-add 'dash-docs-unofficial-docsets :override #'dash-docs-unofficial-docsets+))

(use-package! consult-dash
  :commands (consult-dash)
  :config
  (map! :map consult-dash-embark-keymap
        :n "b" #'browse-url)

  (set-lookup-handlers! 'lsp-mode
    :definition #'+lsp-lookup-definition-handler
    :references #'+lsp-lookup-references-handler
    :documentation #'+consult-dash-doc
    :implementations '(lsp-find-implementation :async t)
    :type-definition #'lsp-find-type-definition)

  ;; (add-hook! 'lsp-mode-hook :append
  ;;   (defun override-js-lookup-handlers-h ()
  ;;     (set-lookup-handlers! '(js-mode rjsx-mode)
  ;;       :documentation #'+consult-dash-doc)))

  ;; (add-hook! '(js-mode-hook rjsx-mode-hook)
  ;;   (defun js-mode-h ()
  ;;     ;; lsp-describe is broken for javascript, use dash instead
  ;;     (setq '+lookup-documentation-functions
  ;;           (delete
  ;;            'lsp-describe-thing-at-point
  ;;            +lookup-documentation-functions))
  ;;     (set-lookup-handlers! '(js-mode rjsx-mode)
  ;;       :documentation #'+consult-dash-doc)))
  )
