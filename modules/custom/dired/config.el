;;; custom/dired/config.el -*- lexical-binding: t; -*-

(use-package! treemacs-icons-dired
  :after dired
  :hook (dired-mode . treemacs-icons-dired-mode)
  :config
  ;; icons in subtrees
  (advice-add 'dired-subtree-insert :after #'treemacs-icons-after-subtree-insert-a))

(use-package! treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        ;; treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (concat doom-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (concat doom-cache-dir "treemacs-last-error-persist"))
  :config
  (map! :map evil-treemacs-state-map
        "o l" #'treemacs-visit-node-horizontal-split)

  (after! dired (treemacs-resize-icons 16))
  (treemacs-follow-mode 1)
  (add-hook! 'treemacs-mode-hook #'hl-line-mode
    (defun treemacs--dont-ignore-winum-h ()
      (setq winum-ignored-buffers-regexp
            (remove (regexp-quote
                     (format "%sScoped-Buffer-"
                             treemacs--buffer-name-prefix))
                    winum-ignored-buffers-regexp))))

  (after! winum
    ;; (map! :map winum-keymap
    ;;  [remap winum-select-window-0] #'treemacs-select-window)
    (setq winum-ignored-buffers-regexp
          (remove ".*Treemacs.*" winum-ignored-buffers-regexp))
    (dolist (wn (seq-map 'number-to-string (number-sequence 0 9)))
      (let ((f (intern (concat "winum-select-window-" wn)))
            (k (concat "s-" wn)))
        (map! :map treemacs-mode-map k f)))))

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
    "o s"    #'treemacs-visit-node-vertical-split
    "L"     (cmd! (treemacs-toggle-node :recursive))))

(use-package! treemacs-projectile
  :after treemacs)

(use-package! lsp-treemacs
  :after (treemacs lsp))


(use-package! dired-imenu
  :after dired)

(use-package! dired-subtree
  :after dired
  :init
  (setq dired-subtree-cycle-depth 5)
  (map! :map dired-mode-map
        :n "M-l" #'dired-subtree-cycle
        :n "M-h" #'dired-subtree-remove*
        :n "M-k" #'dired-subtree-remove*
        :n "M-j" #'dired-subtree-down-n-open
        :n "M-n" #'dired-subtree-next-sibling
        :n "M-p" #'dired-subtree-previous-sibling))

(after! dired
  (map! :map dired-mode-map
        :n "s" #'dired-sort-toggle-or-edit)

  (setq dired-use-ls-dired t
        dired-listing-switches "-alh --group-directories-first"
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer nil
        dired-do-revert-buffer t
        remote-file-name-inhibit-delete-by-moving-to-trash t
        dired-vc-rename-file t)

  (put 'dired-find-alternate-file 'disabled nil)

  (add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "open -a Preview"))

  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls
              dired-listing-switches "-aBhl --group-directories-first"))))

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (when (modulep! :custom general)
    (map!
     :map dired-mode-map
     :n "o" nil
     (:prefix ("o" . "open")
      :desc "below" :n  "j" (dired-split-action +evil/window-split-and-follow)
      :desc "right" :n "l" (dired-split-action +evil/window-vsplit-and-follow)
      :desc "left"  :n "h" (dired-split-action split-window-horizontally)
      :desc "above" :n "k" (dired-split-action split-window-vertically)
      :desc "ace-action" :n "a" #'dired-ace-action)))

  (when (modulep! :custom search)
    (add-hook! dired-after-readin #'+add-to-zoxide-cache)))

(use-package! dired-sidebar
  :defer t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-should-follow-file t
        dired-sidebar-window-fixed nil))

(use-package! dired-narrow
 :after dired
 :config
 (add-hook! 'dired-mode-hook
   (defun dired-narrow-keys-h ()
     (map! :map dired-mode-map
           (:localleader
            "n" #'dired-narrow-fuzzy
            "s" #'dired-sort-toggle-or-edit)))))
