;; -*- no-byte-compile: t; -*-
;;; custom/ai/packages.el

(package! whisper :recipe (:host github :repo "natrys/whisper.el"))

(package! gptel
  :recipe (:host github
           :repo "karthink/gptel"
           :files ("!test/*")))

(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))

(package! aider :recipe (:host github :repo "tninja/aider.el"))

(package! khoj :recipe (:host github :repo "khoj-ai/khoj"))

(package! ob-gptel :recipe (:host github :repo "jwiegley/ob-gptel"))
