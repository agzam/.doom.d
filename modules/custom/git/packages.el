;; -*- no-byte-compile: t; -*-
;;; custom/git/packages.el

(package! magit
  :recipe (:host github :repo "magit/magit"))
(package! compat
  :recipe (:host github :repo "emacs-compat/compat"))

;; (package! emacsql-sqlite-builtin)
(package! forge :recipe (:host github :repo "magit/forge"))

(package! gh-notify :recipe (:host github :repo "anticomputer/gh-notify"))

(package! git-link)

;; @tarsius broke Code-Review: https://github.com/wandersoncferreira/code-review/issues/245
(package! code-review :recipe (:host github :repo "EGmux/code-review"))

;; (package! gist)


(unpin!
 ;; magit
 ;; forge
 ;; compat
 )

(package! closql)

(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh" :files ("*.el")))

(package! git-auto-commit-mode)
