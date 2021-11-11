;; -*- no-byte-compile: t; -*-

;; (package! prescient)
(package! fasd :recipe (:repo "https://framagit.org/dalanicolai/emacs-fasd.git"))
(package! vertico :recipe (:host github :repo "minad/vertico" :files ("*.el" "extensions")))
(package! vertico-posframe)
(unpin! vertico consult)
(package! embark-consult)
(package! consult-projectile)
(package! consult-company)
(package! company-posframe)

(package! winum)
(package! info+)
