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
(package! corfu-terminal
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(package! corfu-doc-terminal
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))

(unpin! vertico consult embark embark-consult)

;; (package! vertico :recipe (:host github :repo "minad/vertico" :files ("*.el" "extensions")))
(package! vertico-posframe :recipe (:host github :repo "tumashu/vertico-posframe"))
(package! consult-projectile)
