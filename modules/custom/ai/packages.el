;; -*- no-byte-compile: t; -*-
;;; custom/ai/packages.el

(package! whisper :recipe (:host github :repo "natrys/whisper.el"))

;;;;;;;;;;;
;; gptel ;;
;;;;;;;;;;;
(package! gptel)
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))
(package! ob-gptel :recipe (:host github :repo "jwiegley/ob-gptel"))
(package! llm-tool-collection :recipe
  (:host github :repo "skissue/llm-tool-collection"))
(package! gptel-tools :recipe
  (:local-repo "gptel-tools" :files ("*.el")))
(package! ragmacs :recipe
  (:host github :repo "positron-solutions/ragmacs"))
(package! gptel-agent
  :recipe (:host github :repo "karthink/gptel-agent"
                 :files ("*.el" "agents")))
(package! gptel-anthropic-oauth :recipe (:local-repo "gptel-anthropic-oauth"))


(package! shell-maker)
(package! acp :recipe (:host github :repo "xenodium/acp.el"))
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell"))

(package! claude-code
  :recipe (:host github :repo "stevemolitor/claude-code.el"
           :files ("*.el" (:exclude "images/*"))))

(package! eca
  :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))


