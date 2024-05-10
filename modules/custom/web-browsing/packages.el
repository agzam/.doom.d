;; -*- no-byte-compile: t; -*-
;;; custom/web-browsing/packages.el

(package! eww :built-in t)
(package! browser-hist :recipe (:local-repo "browser-hist"))

(package! elfeed)
(package! elfeed-goodies)
(package! elfeed-org)
(package! rfc-mode)

(package! consult-web :recipe
  (:host github :repo "armindarvish/consult-web"
   :files (:defaults "sources/*.el")))
