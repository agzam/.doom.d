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

(unpin! compat
        consult
        embark
        embark-consult
        posframe
        vertico
        vertico-posframe)

(package! yasnippet-capf :recipe (:host github :repo "elken/yasnippet-capf"))
