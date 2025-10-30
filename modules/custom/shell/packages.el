;; -*- no-byte-compile: t; -*-
;;; custom/shell/packages.el

(package! shell-pop)
(package! vimrc-mode)

(package! eat)

(package! vterm :recipe
  (:host github
   :repo "akermu/emacs-libvterm"))

(package! eshell-vterm)

(package! eshell-atuin)

(package! yuck-mode)

(package! mise :recipe (:host github
                        :repo "eki3z/mise.el"))
