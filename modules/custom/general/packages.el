;; -*- no-byte-compile: t; -*-

;; (package! prescient)
(package! fasd :recipe (:repo "https://framagit.org/dalanicolai/emacs-fasd.git"))
(package! vertico :recipe (:host github :repo "minad/vertico" :files ("*.el" "extensions")))

(package! vertico-posframe)
(unpin! vertico)
(package! embark-consult)

(package! consult-projectile)
(package! consult-company)

(when (eq system-type 'darwin)
  ;; ln -s ~/.hammerspoon/ ~/.doom.d/modules/custom/general/spacehammer
  (package! spacehammer :recipe (:local-repo "spacehammer" :files ("*.el"))))

(package! winum)
(package! hydra)
