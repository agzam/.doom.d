;; -*- no-byte-compile: t; -*-
;;; custom/chat/packages.el

(package! telega)

(package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("*.el")))
(package! openai :recipe (:host github :repo "emacs-openai/openai" :files ("*.el")))
