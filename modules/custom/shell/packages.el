;; -*- no-byte-compile: t; -*-
;;; custom/shell/packages.el

(package! shell-pop)
(package! vimrc-mode)

(package! ghostel :recipe
  (:host github
   :repo "dakra/ghostel"
   :files ("lisp/*.el")))

(package! evil-ghostel :recipe
  (:host github
   :repo "dakra/ghostel"
   :files ("extensions/evil-ghostel/*.el")))

(package! eshell-atuin)

(package! yuck-mode)

(package! mise :recipe (:host github
                        :repo "eki3z/mise.el"))

(package! kkp)
