;; -*- no-byte-compile: t; -*-
;;; custom/tree-sitter/packages.el

(package! clojure-ts-mode :recipe (:host github :repo "clojure-emacs/clojure-ts-mode"))

;; (package! ts-fold :recipe (:host github :repo "emacs-tree-sitter/ts-fold"))

;; TODO: switch to default after emacs-tree-sitter/ts-fold#48 fixed
(package! treesit-fold :recipe (:host github :repo "abougouffa/treesit-fold"))


(package! evil-matchit :recipe (:host github :repo "redguardtoo/evil-matchit"))
;; (package! elisp-tree-sitter :recipe (:host github :repo "emacs-tree-sitter/elisp-tree-sitter"))

;; (package! tree-sitter-langs :recipe (:host github :repo "emacs-tree-sitter/tree-sitter-langs"))
