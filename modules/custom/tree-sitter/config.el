;;; custom/tree-sitter/config.el -*- lexical-binding: t; -*-

(use-package! treesit
  :defer t
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\Dockerfile\\'" . dockerfile-ts-mode))
  :preface
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             ;; (clojure-mode . clojure-ts-mode)
             (dockerfile-mode . dockerfile-ts-mode)
             (bash-mode . bash-ts-mode)
             (mermaid-mode . mermaid-ts-mode)
             (java-mode . java-ts-mode)
             (go-mode . go-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (tree-sitter-init+))

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
  :after evil
  :config
  (global-evil-matchit-mode 1))

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
