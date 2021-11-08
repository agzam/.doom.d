;; -*- no-byte-compile: t; -*-
;;; custom/version-control/packages.el

(package! magit)
(package! forge)
(package! github-review)
(package! gh-notify :recipe (:host github :repo "anticomputer/gh-notify" :branch "dev"))
(package! git-link)
(package! code-review :recipe (:host github :repo "wandersoncferreira/code-review"))
(package! gist)
