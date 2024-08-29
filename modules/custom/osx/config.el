;;; custom/osx/config.el -*- lexical-binding: t; -*-

(use-package! ob-applescript
  :after org
  :if (featurep :system 'macos)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((applescript . t))))

(use-package! applescript-mode
  :if (featurep :system 'macos)
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'" . applescript-mode))
  :commands (applescript-mode))
