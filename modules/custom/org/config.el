;;; custom/org/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (format "%sstraight/build-%s/org-roam/extensions/"
                                (file-truename doom-local-dir) emacs-version))

(defvar org-default-folder "~/Dropbox/org/")

(global-set-key (kbd "C-c C-f") #'org-roam-node-find)
(global-set-key (kbd "C-c r") #'org-roam-dailies-capture-date)

(use-package! org
  :defer t
  :config
  (setf org-directory org-default-folder)
  (setq
   org-ctrl-k-protect-subtree t
   org-ellipsis " ↴"
   org-catch-invisible-edits 'smart
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts nil
   org-log-into-drawer t
   org-log-states-order-reversed nil)

  (add-to-list
   'auto-mode-alist
   `(,(format "\\%s.*\.txt\\'" (replace-regexp-in-string "~" "" org-default-folder)) . org-mode))

  (setq
   org-confirm-babel-evaluate nil
   org-todo-keywords '((sequence "TODO(t!)" "ONGOING(o!)" "|" "DONE(d)" "CANCELED(c@/!)"))
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t)

  (setq org-link-make-description-function #'+org-link-make-description-function)

  (map! :map org-mode-map
        "C-c C-f" #'org-roam-node-find
        "C-c C-i" #'org-roam-node-insert+
        (:localleader
         "n" #'org-next-link
         "p" #'org-previous-link
         (:prefix ("b" . "babel")
          "k" #'org-babel-remove-result)
         (:prefix ("g" . "goto")
          "L" #'org-goto-last-heading
          "d" #'org-goto-datetree-date)
         (:prefix ("i" . "insert")
          "l" #'org-insert-link
          "L" #'org-cliplink)
         (:prefix ("r" . "roam")
          "i" #'org-roam-node-insert+
          "l" #'org-roam-buffer-toggle
          "w" #'org-roam-toggle-ui-xwidget
          "f" #'org-roam-node-find
          "d" #'org-roam-dailies-find-date)
         (:prefix ("s" . "tree/subtree")
          "a" #'org-toggle-archive-tag
          "A" #'org-archive-subtree
          "j" #'consult-org-heading
          "N" #'widen
          "S" #'org-sort
          "x" #'org-cut-subtree)
         (:prefix ("t" . "toggle")
          "l" #'org-toggle-link-display)))

  (add-hook! 'org-mode-hook
             #'org-indent-mode
             (defun flycheck-mode-off ()
               (flycheck-mode -1)))
  (add-hook! 'org-capture-mode-hook #'recenter)
  (setq org-capture-bookmark nil)

  (after! org-attach
    (add-hook! 'org-attach-after-change-hook
      (defun org-attach-save-file-list-to-property (dir)
        (when-let ((files (org-attach-file-list dir)))
          (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))))))

(use-package! org-tempo
  :after org
  :config
  (add-to-list 'org-modules 'org-tempo t))

(use-package! org-roam
  :commands org-roam-node-find org-roam-dailies-capture-date
  :after org
  :init
  (setq
   org-roam-v2-ack t
   org-roam-directory org-default-folder
   org-roam-db-location (concat org-default-folder "org-roam.db")
   org-roam-dailies-directory "daily/")
  :config
  (map! :map org-mode-map
        :i "[[" #'org-roam-node-insert
        :i "[ SPC" (cmd! (insert "[]")
                         (backward-char)))
  (map! :map org-roam-mode-map
        "C-c i" #'org-roam-node-insert+
        "RET" (cmd! (org-roam-node-visit (org-roam-node-at-point) :other-window))
        (:prefix ("g" . "goto")
         "k" #'org-backward-element
         "j" #'org-forward-element)
        (:localleader
         "rf" #'org-roam-node-find
         "rl" #'org-roam-buffer-toggle))

  (setq
   org-roam-completion-everywhere t
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new
           (file+head
            "${slug}.org"
            "\n#+title: ${title}\n")
           :unnarrowed t
           :jump-to-captured nil)))

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :if-new
           (file+head "${slug}.org" "#+title: ${title}\n%(org-roam--link-to \"unread\")")
           :unnarrowed t
           :jump-to-captured t)
          ("n" "non-browser" plain "%?" :if-new
           (file+head "read-later/${slug}.org" "#+title: ${title}\n%(org--insert-selection-dwim \"${body}\")")
           :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("w" "work" plain
           "%(org-roam-capture-dailies--set-node-props \"work\")%?"
           :if-new
           (file+datetree
            "%<%Y-%m>-work-notes.org"
            'day)
           :jump-to-captured t
           :unnarrowed t)
          ("j" "journal" plain
           "%(org-roam-capture-dailies--set-node-props \"journal\")%?"
           :if-new
           (file+datetree
            "%<%Y-%m>-journal.org"
            'day)
           :jump-to-captured t
           :unnarrowed t)))

  (org-roam-db-autosync-mode +1)

  (add-to-list
   'display-buffer-alist
   '("\\*org-roam\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.2))))

(use-package! org-roam-protocol
  :after org-roam)

(use-package! org-roam-dailies
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :init
  (setq org-roam-ui-port 8081
        org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*org-roam-ui\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.4)))

  (add-hook! 'org-mode-hook
    (defun org-roam-ui-on ()
      (unless org-roam-ui-mode
        (org-roam-ui-mode +1)))))

(use-package! evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :hook (org-capture-mode . evil-insert-state)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme)
  (add-hook! 'org-tab-first-hook :append
             ;; Only fold the current tree, rather than recursively
             #'+org-cycle-only-current-subtree-h
             ;; Clear babel results if point is inside a src block
             #'+org-clear-babel-results-h)
  (let-alist evil-org-movement-bindings
    (let ((Cright  (concat "C-" .right))
          (Cleft   (concat "C-" .left))
          (Cup     (concat "C-" .up))
          (Cdown   (concat "C-" .down))
          (CSright (concat "C-S-" .right))
          (CSleft  (concat "C-S-" .left))
          (CSup    (concat "C-S-" .up))
          (CSdown  (concat "C-S-" .down)))
      (map! :map evil-org-mode-map
            :ni [C-return]   #'+org/insert-item-below
            :ni [C-S-return] #'+org/insert-item-above
            ;; navigate table cells (from insert-mode)
            :i Cright (cmds! (org-at-table-p) #'org-table-next-field
                             #'recenter-top-bottom)
            :i Cleft  (cmds! (org-at-table-p) #'org-table-previous-field)
            :i Cup    (cmds! (org-at-table-p) #'+org/table-previous-row
                             #'org-up-element)
            :i Cdown  (cmds! (org-at-table-p) #'org-table-next-row
                             #'org-down-element)
            :ni CSright   #'org-shiftright
            :ni CSleft    #'org-shiftleft
            :ni CSup      #'org-shiftup
            :ni CSdown    #'org-shiftdown
            ;; more intuitive RET keybinds
            :n [return]   #'+org/dwim-at-point
            :n "RET"      #'+org/dwim-at-point
            :i [return]   (cmd! (org-return electric-indent-mode))
            :i "RET"      (cmd! (org-return electric-indent-mode))
            :i [S-return] #'+org/shift-return
            :i "S-RET"    #'+org/shift-return
            ;; more vim-esque org motion keys (not covered by evil-org-mode)
            :m "]h"  #'org-forward-heading-same-level
            :m "[h"  #'org-backward-heading-same-level
            :m "]l"  #'org-next-link
            :m "[l"  #'org-previous-link
            :m "]c"  #'org-babel-next-src-block
            :m "[c"  #'org-babel-previous-src-block
            :n "gQ"  #'org-fill-paragraph
            ;; sensible vim-esque folding keybinds
            :n "za"  #'+org/toggle-fold
            :n "zA"  #'org-shifttab
            :n "zc"  #'+org/close-fold
            :n "zC"  #'outline-hide-subtree
            :n "zm"  #'+org/hide-next-fold-level
            :n "zM"  #'+org/close-all-folds
            :n "zn"  #'org-tree-to-indirect-buffer
            :n "zo"  #'+org/open-fold
            :n "zO"  #'outline-show-subtree
            :n "zr"  #'+org/show-next-fold-level
            :n "zR"  #'+org/open-all-folds
            :n "zi"  #'org-toggle-inline-images

            :map org-read-date-minibuffer-local-map
            Cleft    (cmd! (org-eval-in-calendar '(calendar-backward-day 1)))
            Cright   (cmd! (org-eval-in-calendar '(calendar-forward-day 1)))
            Cup      (cmd! (org-eval-in-calendar '(calendar-backward-week 1)))
            Cdown    (cmd! (org-eval-in-calendar '(calendar-forward-week 1)))
            CSleft   (cmd! (org-eval-in-calendar '(calendar-backward-month 1)))
            CSright  (cmd! (org-eval-in-calendar '(calendar-forward-month 1)))
            CSup     (cmd! (org-eval-in-calendar '(calendar-backward-year 1)))
            CSdown   (cmd! (org-eval-in-calendar '(calendar-forward-year 1)))))))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-delay 0.5
        org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autosubmarkers t))

(use-package! org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-hide-leading-stars nil
        org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("[ ]"  . 9744)
                                          ("DONE" . 9745)
                                          ("[X]"  . 9745))
        org-superstar-item-bullet-alist '((?* . ?⋆)
                                          (?+ . ?◦)
                                          (?- . ?•))))

(use-package! org-edit-indirect
  :after org)
