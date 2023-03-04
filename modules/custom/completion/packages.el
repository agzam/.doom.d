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

(unpin! compat
        consult
        embark
        embark-consult
        vertico)

;; (package! vertico :recipe (:host github :repo "minad/vertico" :files ("*.el" "extensions")))
(package! vertico-posframe :recipe (:host github :repo "tumashu/vertico-posframe"))
(package! consult-projectile)
