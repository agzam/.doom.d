;; -*- no-byte-compile: t; -*-
;;; custom/tree-sitter/packages.el

(package! clojure-ts-mode :recipe (:host github :repo "clojure-emacs/clojure-ts-mode"))

;; (package! treesit-fold :recipe (:host github :repo "emacs/tree-sitter/treesit-fold"))

(package! treesitter-context :recipe (:host github :repo "zbelial/treesitter-context.el"))

(package! evil-matchit :recipe (:host github :repo "redguardtoo/evil-matchit"))
;; (package! elisp-tree-sitter :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter"))

;; (package! tree-sitter-langs :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs"))
