;;; custom/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package! treesit
  :when (treesit-available-p)
  :config
  ;; Emacs 31: built-in *-ts-modes register their own grammar source and
  ;; install it on first visit, so the manual source-alist + install loop is
  ;; gone. Explicit list (not `t') so we don't clobber yaml-mode (yaml-pro,
  ;; highlight-indent-guides) or lua-mode, which hook the non-ts major modes.
  (setopt treesit-auto-install-grammar 'ask
          treesit-enabled-modes '(python-ts-mode css-ts-mode
                                  typescript-ts-mode js-ts-mode json-ts-mode
                                  bash-ts-mode dockerfile-ts-mode
                                  java-ts-mode go-ts-mode))
  ;; .json opens in third-party json-mode, not in the core registry.
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(mermaid-mode . mermaid-ts-mode))
  ;; yaml-pro-ts-mode (a minor mode on yaml-mode) and the third-party
  ;; mermaid-ts-mode consume a grammar but, unlike built-in *-ts-modes, don't
  ;; register a source or self-install. Ensure those two; a no-op once present.
  (dolist (src '((yaml    "https://github.com/ikatyang/tree-sitter-yaml" "master")
                 (mermaid "https://github.com/monaqa/tree-sitter-mermaid")))
    (add-to-list 'treesit-language-source-alist src))
  (dolist (lang '(yaml mermaid))
    (treesit-ensure-installed lang)))

;; (use-package! clojure-ts-mode
;;   :after treesit)

;; (use-package! ts-fold
;;   :after tree-sitter
;;   :config
;;   ;; we want to use our own face so we nullify this one to have no effect and
;;   ;; make it more similar to hideshows
;;   (custom-set-faces! '(ts-fold-replacement-face :foreground unspecified
;;                        :box nil
;;                        :inherit font-lock-comment-face
;;                        :weight light))
;;   (setq ts-fold-replacement "  [...]  ")
;;   (ts-fold-mode +1))

(use-package! evil-matchit
  :defer t
  :commands (global-evil-matchit-mode)
  :init
  (add-hook 'doom-first-file-hook #'global-evil-matchit-mode))

(use-package! mermaid-mode
  :defer t
  :config
  ;; install https://github.com/mermaid-js/mermaid-cli
  (when-let* ((mmdc (executable-find "mmdc")))
    (setopt mermaid-mmdc-location mmdc)))

(use-package! treesitter-context
  :config
  (eval `(add-hook! ,treesitter-context--fold-supported-mode
                    #'treesitter-context-fold-mode))

  (defadvice! fold-all-with-treesitter-context-a (orig-fn lst action)
    "hack to make treesitter-context-fold work with fold/hide all."
    :around #'evil-fold-action
    (if (and (member major-mode treesitter-context--fold-supported-mode)
             treesitter-context-fold-mode
             (member action '(:close-all :open-all)))
        (save-excursion
          (goto-char (point-max))
          (while (treesit-beginning-of-defun 1)
            (pcase action
              (:close-all (treesitter-context-fold-hide))
              (:open-all  (treesitter-context-fold-show)))))
      (funcall orig-fn lst action))))
