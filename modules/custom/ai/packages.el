;; -*- no-byte-compile: t; -*-
;;; custom/ai/packages.el

(package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("*.el")))
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
(package! gptel)
