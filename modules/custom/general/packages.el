;; -*- no-byte-compile: t; -*-

(package! fasd :recipe (:repo "https://framagit.org/dalanicolai/emacs-fasd.git"))
(unpin! vertico consult)
(package! vertico :recipe (:host github :repo "minad/vertico" :files ("*.el" "extensions")))
(package! vertico-posframe)
(package! embark-consult)
(package! consult-projectile)
(package! consult-company)
(package! company-posframe)

(package! winum)
(package! info+)
(package! hydra)
