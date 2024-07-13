;; -*- no-byte-compile: t; -*-
;;; custom/web-browsing/packages.el

(package! eww :built-in t)
(package! browser-hist :recipe (:local-repo "browser-hist"))

(package! elfeed)
;; (package! elfeed-goodies)
(package! elfeed-org)
(package! elfeed-tube)
(package! elfeed-tube-mpv)

(package! rfc-mode)

(package! yeetube
  :recipe (:type git :repo "https://git.thanosapollo.org/yeetube"))

(package! consult-omni :recipe
  (:host github :repo "armindarvish/consult-omni"
   :branch "develop"
   :files (:defaults "sources/*.el")))
