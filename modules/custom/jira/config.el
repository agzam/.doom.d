;;; custom/jira/config.el -*- lexical-binding: t; -*-

(setopt jira-default-search-format-string
        "project = SAC AND status NOT IN (Closed, Done) AND text ~ \"%s\"")

(map! :map jira-browse-ticket-mode-map
      :n "q" #'kill-buffer-and-window)

(after! embark
  (defvar-keymap embark-jira-ticket-map
    :doc "Keymap for Jira ticket actions."
    :parent embark-general-map)

  (dolist (finder '(+embark-target-jira-ticket-at-point))
    (add-to-list 'embark-target-finders finder))

  (add-to-list 'embark-keymap-alist '(jira-ticket embark-jira-ticket-map))

  (map! :map embark-jira-ticket-map
        (:prefix ("b" . "browse")
         :desc "view" "b" #'jira-view-simple
         :desc "in browser" "o" #'jira-browse-ticket-url)
        (:prefix ("f" . "find")
         :desc "GH PRs" "g" #'jira-find-pull-requests-on-github)
        (:prefix ("c" . "convert")
         :desc "link" "l" #'jira-ticket->link
         :desc "link+desc" "d" #'jira-ticket->num+description)))

(map! :map jira-browse-ticket-mode-map
      :n "yy" #'jira--browser-ticket-mode-get-url)
