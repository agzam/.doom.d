;; -*- no-byte-compile: t; -*-
;;; custom/version-control/packages.el

(package! magit)
(package! forge)
(package! github-review)
(package! gh-notify :recipe (:host github :repo "anticomputer/gh-notify" :branch "dev"))
(package! git-link)
(package! code-review :recipe (:host github :repo "wandersoncferreira/code-review"))
(package! gist)

(package! transient :recipe (:host github :repo "magit/transient" )
  ;; update breaks my transients, see: https://github.com/magit/transient/issues/219
  :pin "0a3b22f169b84ab7a51dc83856b0b6487fdf41da"
  )
