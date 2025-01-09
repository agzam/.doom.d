;; -*- no-byte-compile: t; -*-
;;; custom/shell/packages.el

(package! shell-pop)
(package! vimrc-mode)
(package! system-packages)

(package! eat :recipe
  (:host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))

(package! vterm :recipe
  (:host github
   :repo "akermu/emacs-libvterm"))


(package! eshell-vterm)
