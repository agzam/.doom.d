;; -*- no-byte-compile: t; -*-
;;; custom/web-browsing/packages.el

(package! eww :built-in t)
(package! hnreader)
(package! browser-hist :recipe (:local-repo "browser-hist"))

(package! elfeed)
(package! elfeed-goodies)
(package! elfeed-org)
