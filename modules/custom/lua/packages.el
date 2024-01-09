;; -*- no-byte-compile: t; -*-
;;; custom/lua/packages.el

(package! lua-mode)
(package! fennel-mode)

(package! friar
  :recipe (:host github :repo "warreq/friar"
           :branch "master"
           :files (:defaults "*.lua" "*.fnl")))
