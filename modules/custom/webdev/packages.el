;; -*- no-byte-compile: t; -*-
;;; custom/webdev/packages.el

(package! emmet-mode)
;; (package! rjsx-mode)
(package! prettier-js)

(when (modulep! :tools lookup)
  (package! xref-js2))

(package! html-to-hiccup)

(package! jtsx)
(package! js-comint)
