;; -*- no-byte-compile: t; -*-
(unpin! vertico consult embark embark-consult)
(package! fasd :recipe (:repo "https://framagit.org/dalanicolai/emacs-fasd.git"))

;; (package! vertico :recipe (:host github :repo "minad/vertico" :files ("*.el" "extensions")))
(package! vertico-posframe)

(package! consult-projectile)
(package! consult-company)
(package! company-posframe)

(package! winum)
(package! info+)
(package! hydra)
