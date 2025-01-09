;; -*- no-byte-compile: t; -*-
;;; custom/completion/packages.el

(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))

(package! corfu-terminal
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(when (modulep! +icons)
  (package! kind-icon))
(package! orderless)
(package! cape)
(package! popon
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
(package! compat)

(package! consult-projectile)
(package! yasnippet)
(package! consult-yasnippet)

(package! dash-docs)
(package! consult-dash)

(package! consult
  :pin "0d0ae4659ecaa3c12927fa3982c49610da573a88" ; until armindarvish/consult-omni#47 is fixed
  :recipe (:host github :repo "minad/consult")) 

(unpin! compat
        ;; consult
        embark
        embark-consult
        posframe
        vertico
        vertico-posframe)

(package! yasnippet-capf :recipe (:host github :repo "elken/yasnippet-capf"))
