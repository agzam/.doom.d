;; -*- no-byte-compile: t; -*-
;;; custom/colors/packages.el

(package! circadian)
(package! spacemacs-theme :recipe (:host github :repo "nashamri/spacemacs-theme"))
(package! base16-theme)

(package! ag-themes :recipe (:local-repo "ag-themes" :files ("*.el")))
(package! rainbow-mode)
