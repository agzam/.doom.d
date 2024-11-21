;;; custom/jira/config.el -*- lexical-binding: t; -*-

(setq jira-default-search-format-string
      "project=TDL AND text ~ \"%s\"")

(map! :map jira-browse-ticket-mode-map
      :n "q" #'kill-buffer-and-window)
