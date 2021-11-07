;;; custom/dired/config.el -*- lexical-binding: t; -*-

(use-package! treemacs-all-the-icons
  :hook '((treemacs-mode dired-mode) . (lambda () (treemacs-load-theme 'all-the-icons))))

(use-package! treemacs-icons-dired
  :hook '(dired-mode . treemacs-icons-dired-mode))

(use-package! treemacs
  :config
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons")
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

        ))

(use-package! dired-imenu
  :after (dired))

(use-package! dired-subtree
  :config
  (map! :map dired-mode-map
        :n "M-l" #'dired-subtree-cycle
        :n "M-h" #'dired-subtree-remove*
        :n "M-k" #'dired-subtree-remove*
        :n "M-j" #'dired-subtree-down-n-open
        :n "M-n" #'dired-subtree-next-sibling
        :n "M-p" #'dired-subtree-previous-sibling))

(after! projectile
  (map! :leader "pd" #'projectile-find-dir))

(after! dired
  (setq dired-use-ls-dired t
        dired-dwim-target t)

  (put 'dired-find-alternate-file 'disabled nil)

  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls
              dired-listing-switches "-aBhl --group-directories-first"))))

  (add-hook 'dired-mode-hook #'dired-hide-details-mode))
