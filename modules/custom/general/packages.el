;; -*- no-byte-compile: t; -*-
(unpin! vertico consult embark embark-consult)

;; (package! vertico :recipe (:host github :repo "minad/vertico" :files ("*.el" "extensions")))
(package! vertico-posframe :recipe (:host github :repo "tumashu/vertico-posframe"))
(package! consult-projectile)

(package! winum)
(package! info+)
(package! hydra)

(package! expand-region)

(unpin! wgrep)
