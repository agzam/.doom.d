;;; custom/jira/config.el -*- lexical-binding: t; -*-

(setq jira-default-search-format-string
      "project=TDL AND text ~ \"%s\"")

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
       :desc "browse" "b" #'jira-view-simple
       (:prefix ("c" . "convert")
        :desc "link" "l" #'jira-ticket->link
        :desc "link+desc" "d" #'jira-ticket->num+description)))
