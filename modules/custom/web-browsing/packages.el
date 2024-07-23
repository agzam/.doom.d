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
  :recipe
  ;; (:type git :repo "https://git.thanosapollo.org/yeetube")
  (:host github :repo "Boruch-Baum/emacs-yeetube.el"))

(package! subed :recipe (:host github :repo "sachac/subed" :files ("subed/*.el")))
