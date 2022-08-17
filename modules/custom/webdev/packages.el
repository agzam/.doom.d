;; -*- no-byte-compile: t; -*-
;;; custom/webdev/packages.el

(package! emmet-mode)
(package! rjsx-mode)
(package! prettier-js)

(when (featurep! :tools lookup)
  (package! xref-js2))
