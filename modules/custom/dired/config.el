;;; custom/dired/config.el -*- lexical-binding: t; -*-

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(use-package! treemacs-all-the-icons
  :hook '((treemacs-mode dired-mode) . (lambda () (treemacs-load-theme 'all-the-icons))))

(use-package! treemacs-icons-dired
  :hook '(dired-mode . treemacs-icons-dired-mode))

(use-package! treemacs
  :config
  (treemacs-resize-icons 16))

(use-package! direx
  :init
  (require 'direx-project)
  :config
  (map! :leader "pt" #'direx/jump-to-project-root-or-current-dir)

  (map! :map direx:file-keymap
        "q" #'kill-this-buffer
        "R" #'direx:do-rename-file
        "C" #'direx:do-copy-files
        "D" #'direx:do-delete-files
        "+" #'direx:create-directory
        "T" #'direx:do-touch
        "j" #'direx:next-item
        "J" #'direx:next-sibling-item
        "k" #'direx:previous-item
        "K" #'direx:previous-sibling-item
        "h" #'direx:collapse-item
        "H" #'direx:collapse-item-recursively
        "l" #'direx:expand-item
        "L" #'direx:expand-item-recursively
        "RET" #'direx:maybe-find-item
        "a" #'direx:find-item
        "r" #'direx:refresh-whole-tree
        "O" #'direx:find-item-other-window
        "|" #'direx:fit-window
        "<C-return>" #'direx:set-root
        "^" #'direx:expand-root-to-parent
        ;; "o" #'spacemacs/dired-open-item-other-window-transient-state/body

        )

  )

(after! projectile
 (map! :leader "pd" #'projectile-find-dir))
