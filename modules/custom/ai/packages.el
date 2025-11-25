;; -*- no-byte-compile: t; -*-
;;; custom/ai/packages.el

(package! whisper :recipe (:host github :repo "natrys/whisper.el"))

(package! gptel)

;; (package! gptel
;;   :recipe (:host github :repo "agzam/gptel" :files ("*.el")))

(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))

(package! ob-gptel :recipe (:host github :repo "jwiegley/ob-gptel"))

(package! llm-tool-collection :recipe
  (:host github :repo "skissue/llm-tool-collection"))

(package! gptel-tools :recipe
  (:local-repo "gptel-tools" :files ("*.el")))

(package! ragmacs :recipe
  (:host github :repo "positron-solutions/ragmacs"))

(package! shell-maker)
(package! acp :recipe (:host github :repo "xenodium/acp.el"))
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell"))

(package! claude-code
  :recipe (:host github :repo "stevemolitor/claude-code.el"
           :files ("*.el" (:exclude "images/*"))))

(package! eca
  :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
