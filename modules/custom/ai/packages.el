;; -*- no-byte-compile: t; -*-
;;; custom/ai/packages.el

(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
(package! gptel :recipe (:host github :repo "karthink/gptel" :files ("*.el")))
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))
