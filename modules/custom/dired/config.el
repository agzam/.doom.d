;;; custom/dired/config.el -*- lexical-binding: t; -*-

(use-package! treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package! treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        ;; treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (concat doom-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat doom-cache-dir "treemacs-last-error-persist"))
  :config
  (map! :leader "ft" #'treemacs)
  (after! dired (treemacs-resize-icons 16))
  (treemacs-follow-mode 1)

  (after! winum
    (map! :map winum-keymap
     [remap winum-select-window-0] #'treemacs-select-window)))

(use-package! treemacs-evil
  :defer t
  :init
  (after! treemacs (require 'treemacs-evil))
  (add-to-list 'doom-evil-state-alist '(?T . treemacs))
  :config
  (define-key! evil-treemacs-state-map
    [return] #'treemacs-RET-action
    [tab]    #'treemacs-TAB-action
    "TAB"    #'treemacs-TAB-action
    ;; REVIEW Fix #1875 to be consistent with C-w {v,s}, but this should really
    ;;        be considered upstream.
    "o v"    #'treemacs-visit-node-horizontal-split
    "o s"    #'treemacs-visit-node-vertical-split))

(use-package! treemacs-projectile
  :after treemacs)

(use-package! lsp-treemacs
  :after (treemacs lsp))

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
  :init
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
  (map! :leader :n "fj" #'dired-jump)

  (setq dired-use-ls-dired t
        dired-dwim-target t)

  (put 'dired-find-alternate-file 'disabled nil)

  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls
              dired-listing-switches "-aBhl --group-directories-first"))))

  (add-hook 'dired-mode-hook #'dired-hide-details-mode))
