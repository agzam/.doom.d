;; -*- no-byte-compile: t; -*-
;;; custom/git/packages.el

(package! magit
  :recipe (:host github :repo "magit/magit"))
(package! compat
  :recipe (:host github :repo "emacs-compat/compat"))

;; (package! emacsql-sqlite-builtin)
(package! forge :recipe (:host github :repo "magit/forge")
  :pin "716bc8fffba81d57e4d349680e94892352cb6175")
(package! transient :recipe (:host github :repo "magit/transient" ))

(package! gh-notify :recipe (:host github :repo "anticomputer/gh-notify"))

(package! git-link)
;; (package! code-review :recipe (:host github :repo "wandersoncferreira/code-review")
;; 	  :pin "26f426e99221a1f9356aabf874513e9105b68140")

;; @tarsius broke Code-Review: https://github.com/wandersoncferreira/code-review/issues/245
(package! code-review :recipe (:host github :repo "phelrine/code-review" :branch "fix/closql-update"))

;; (package! gist)


(unpin!
 ;; magit
 ;; forge
 ;; compat
 )

(package! closql)

(package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh"))

(package! git-auto-commit-mode)
