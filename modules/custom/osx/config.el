;;; custom/osx/config.el -*- lexical-binding: t; -*-

(use-package! ob-applescript
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((applescript . t))))

(use-package! applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'" . applescript-mode))
  :commands (applescript-mode))
