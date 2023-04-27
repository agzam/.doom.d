;; -*- no-byte-compile: t; -*-
;;; custom/chat/packages.el

(package! telega)

(package! gptel :recipe (:host github :repo "karthink/gptel"))
(package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("*.el")))
