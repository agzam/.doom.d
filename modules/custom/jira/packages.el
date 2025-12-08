;;; custom/jira/packages.el -*- lexical-binding: t; -*-

;; Posframe is required for GUI popup support
;; Falls back to eldoc gracefully if not available
(package! posframe)

(package! go-jira :recipe
  (:recipe (:host github :repo "agzam/go-jira.el" :files ("*.el")))
  ;; (:local-repo "go-jira" :files ("*.el"))
  )
