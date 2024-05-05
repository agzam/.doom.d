;; -*- no-byte-compile: t; -*-
;;; custom/completion/packages.el

(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))
(when (modulep! +icons)
  (package! kind-icon))
(package! orderless)
(package! cape)
(package! popon
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
(package! compat)

;; (package! vertico :recipe (:host github
;;                            :repo "minad/vertico"
;;                            :files ("*.el" "extensions")))

;; (package! vertico-posframe :recipe (:host github :repo "agzam/vertico-posframe")
;; 	  :pin "a67689ee24705e4426ddfa363bf3b18ef1e3475b")

(package! consult-projectile)
(package! yasnippet)

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
