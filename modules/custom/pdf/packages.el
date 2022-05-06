;; -*- no-byte-compile: t; -*-
;;; custom/pdf/packages.el

(package! pdf-tools :recipe
  (:host github
   :repo "dalanicolai/pdf-tools"
   :branch "continuous-scroll-version-3"
   :files ("lisp/*.el"
           "README"
           "vimura-server/*.py"
           ("build" "Makefile")
           ("build" "server")
           (:exclude "lisp/tablist.el" "lisp/tablist-filter.el"))))

(package! saveplace-pdf-view)
