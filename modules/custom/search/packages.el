;; -*- no-byte-compile: t; -*-
;;; custom/search/packages.el

(package! consult-omni :recipe
  (:host github :repo "armindarvish/consult-omni"
   :branch "develop"
   :files (:defaults "sources/*.el")))
