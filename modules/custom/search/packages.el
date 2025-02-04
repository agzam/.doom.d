;; -*- no-byte-compile: t; -*-
;;; custom/search/packages.el

(package! consult-omni :recipe
  (:host github :repo "armindarvish/consult-omni"
   :branch "develop"
   :files (:defaults "sources/*.el"))
  :pin "2398ddb53d5f1aa965ab889ed356b6e06624aa27")
