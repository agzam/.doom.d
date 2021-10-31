;; -*- no-byte-compile: t; -*-

(package! helpful)
(package! prescient)
(package! fasd :recipe (:repo "https://framagit.org/dalanicolai/emacs-fasd.git"))
(package! vertico
  :recipe (:host github :repo "minad/vertico"))
(package! embark-consult)

(unpin! vertico)
