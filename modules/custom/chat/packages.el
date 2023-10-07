;; -*- no-byte-compile: t; -*-
;;; custom/chat/packages.el

(package! telega)

(package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("*.el")))
