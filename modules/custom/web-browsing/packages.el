;; -*- no-byte-compile: t; -*-
;;; custom/web-browsing/packages.el

(package! eww :built-in t)
(package! browser-hist :recipe (:local-repo "browser-hist"))

(package! elfeed :recipe (:host github :repo "SohumB/elfeed" :branch "bugfix/consolidate-doesnt-preserve-shape"))
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

(package! hnreader :recipe (:host github :repo "thanhvg/emacs-hnreader"))
(package! consult-hn :recipe (:local-repo "consult-hn"))
(package! reddigg :recipe (:host github :repo "thanhvg/emacs-reddigg"))
