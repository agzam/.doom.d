;; -*- no-byte-compile: t; -*-
;;; custom/completion/packages.el

(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))

(package! corfu-terminal :recipe (:host github :repo "cimisc/emacs-corfu-terminal"))

(when (modulep! +icons)
  (package! kind-icon))
(package! orderless)
(package! cape)
(package! popon :recipe (:host github :repo "cimisc/emacs-popon"))
(package! compat)

(package! consult-projectile)
(package! yasnippet)
(package! consult-yasnippet)

(package! dash-docs)
(package! consult-dash :recipe (:host github :repo "emacsmirror/consult-dash"))

(package! consult :recipe (:host github :repo "minad/consult"))

(unpin! compat
        consult
        posframe
        vertico
        vertico-posframe)

(package! yasnippet-capf :recipe (:host github :repo "elken/yasnippet-capf"))
