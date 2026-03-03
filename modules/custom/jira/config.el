;;; custom/jira/config.el -*- lexical-binding: t; -*-

(use-package! go-jira
  :after (org)
  :config

  (setopt go-jira-default-search-format-string
          "project = SAC AND status NOT IN (Closed, Done) AND text ~ \"%s\"")

  (map! :map go-jira-browse-ticket-mode-map
        :nv "q" #'kill-buffer-and-window
        :nv "yy" #'go-jira--browser-ticket-mode-get-url)

  (add-hook! '(org-mode-hook
               markdown-mode-hook
               prog-mode-hook
               text-mode-hook)
             #'go-jira-enable-popup+eldoc)

  (add-hook! 'go-jira-view-mode-hook
    (defun go-jira-view-mode-h ()
      (map!
       :map go-jira-view-mode-map
       :nv "E" #'go-jira-edit
       :nv "q" #'kill-buffer-and-window
       :nv "gr" #'go-jira-view-mode-refresh)))

  (add-hook! 'go-jira-board-view-mode-hook
    (defun go-jira-board-view-mode-h ()
      (map!
       :map go-jira-board-view-mode-map
       :nv "q" #'kill-buffer-and-window
       :nv "E" #'go-jira-edit
       :nv "gr" #'go-jira-board-refresh)))

  (after! embark
    (map! :map go-jira-embark-jira-ticket-map
          (:prefix ("b" . "browse")
           :desc "view" "b" #'go-jira-view-ticket
           :desc "in browser" "o" #'go-jira-browse-ticket-url)
          (:prefix ("f" . "find")
           :desc "GH PRs" "g" #'go-jira-find-pull-requests-on-github)
          (:prefix ("c" . "convert")
           :desc "link" "l" #'go-jira-ticket->link
           :desc "link+desc" "d" #'go-jira-ticket->num+description
           :desc "git branch" "g" #'go-jira-ticket->git-branch-name))))
