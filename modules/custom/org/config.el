;;; custom/org/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (format "%sstraight/build-%s/org-roam/extensions/"
                                (file-truename doom-local-dir) emacs-version))

(defvar org-default-folder (expand-file-name "~/Sync/org/"))

(use-package! org
  :defer t
  :config
  (setopt org-directory org-default-folder)
  (setopt
   org-ctrl-k-protect-subtree t
   org-ellipsis " ↴"
   org-fold-catch-invisible-edits 'smart
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts nil
   org-log-into-drawer t
   org-log-states-order-reversed nil
   org-cycle-emulate-tab nil
   org-edit-src-content-indentation 0
   org-fontify-quote-and-verse-blocks t
   org-image-actual-width '(0.7))

  (add-to-list
   'auto-mode-alist
   `(,(format "\\%s.*\.txt\\'" (replace-regexp-in-string "~" "" org-default-folder)) . org-mode))

  (setopt
   org-confirm-babel-evaluate nil
   org-todo-keywords '((sequence "TODO(t!)" "ONGOING(o!)" "|" "DONE(d!)" "CANCELED(c@/!)"))
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t)

  (setopt org-link-make-description-function #'+org-link-make-description-function)

  (add-hook! org-mode
    (defun org-init-keybinds-h ()
      (map! :map org-mode-map
            :n "[[" #'org-previous-visible-heading
            :n "]]" #'org-next-visible-heading
            [remap imenu] #'consult-outline
            "C-c C-f f" #'org-roam-node-find
            "C-c C-i" #'org-roam-node-insert+
            :n "zk" #'text-scale-increase

            ;; tilde insead of backtick
            :iv "`" (cmd! (self-insert-command 1 126))

            (:localleader
             (:when (modulep! :completion vertico)
               "." #'consult-org-heading)
             (:prefix ("b" . "babel")
                      "k" #'org-babel-remove-result)
             (:prefix ("d" . "date")
                      "t" #'+org-goto-datetree-date)
             (:prefix ("g" . "goto")
              :desc "final heading" "L" #'+org-goto-bottommost-heading)
             (:prefix ("i" . "insert")
                      "l" #'org-insert-link
                      "L" #'org-cliplink
                      "c" #'yank-from-clipboard)
             (:prefix ("l" . "links")
                      "i" #'org-id-store-link
                      "n" #'org-next-link
                      "p" #'org-previous-link)
             "n" #'org-noter-transient
             (:prefix ("o" . "open/Org")
                      "l" #'org-id-store-link
                      "L" #'org-store-link-id-optional)
             (:prefix ("r" . "roam")
              "b" #'consult-org-roam-backlinks
              "i" #'org-roam-node-insert+
              "l" #'org-roam-buffer-toggle
              :desc "org-roam-ui in xwidget" "w" #'org-roam-toggle-ui-xwidget
              :desc "org-roam-ui in browser" "W" #'org-roam-ui-browser+
              "f" #'org-roam-node-find
              "F" #'consult-org-roam-forward-links
              "d" #'org-roam-dailies-find-date
              (:prefix ("r" . "refile")
                       "n" #'org-roam-refile-to-node))
             (:prefix ("s" . "tree/subtree")
                      "a" #'org-toggle-archive-tag
                      "A" #'org-archive-subtree
                      "j" #'consult-org-heading
                      "n" #'org-narrow-to-subtree
                      "N" #'widen
                      "S" #'org-sort
                      "x" #'org-cut-subtree)
             (:prefix ("t" . "toggle")
                      "l" #'org-toggle-link-display)))))

  (map! :map org-agenda-mode-map
        :n "RET" #'org-agenda-switch-to)

  ;; (add-hook! 'org-tab-first-hook
  ;;            #'+org-yas-expand-maybe-h
  ;;            #'+org-indent-maybe-h)

  (add-hook!
   'org-mode-hook
   #'org-indent-mode
   #'yas-minor-mode-on
   #'org-roam-count-overlay-mode
   #'bug-reference-mode
   (defun flycheck-disable-h () (flycheck-mode -1)))

  (add-hook! 'org-capture-mode-hook #'recenter)

  (setopt org-export-with-smart-quotes nil
          org-html-validation-link nil
          org-latex-prefer-user-labels t
          org-ascii-text-width 900 ; don't wrap text
          org-ascii-links-to-notes nil)
  (add-to-list 'org-export-backends 'md)

  (setopt org-capture-bookmark nil)

  (after! org-attach
    (add-hook! 'org-attach-after-change-hook
      (defun org-attach-save-file-list-to-property (dir)
        (when-let ((files (org-attach-file-list dir)))
          (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", ")))))

    (advice-add 'org--image-yank-media-handler
                :around #'yank-media--tiff-as-png-a))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (js . t)
     (python . t)
     (clojure . t)
     (sql . t)
     (sqlite . t)
     ;; (ditaa . t)
     ;; (ruby . t)
     ))

  ;; youtube videos played in mpv
  (org-link-set-parameters
   "yt" :follow (lambda (path) (mpv-open+ (concat "https:" path)))
   :export (lambda (link _desc _format)
             (format
              (concat
               "<iframe width=\"560\" height=\"315\" src=\"https:%s\" title=\"YouTube video player\" frameborder=\"0\" "
               "allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share\" "
               "referrerpolicy=\"strict-origin-when-cross-origin\" allowfullscreen></iframe>")
              link))))

(use-package! org-tempo
  :after org
  :config
  (add-to-list 'org-modules 'org-tempo t))

(use-package! org-roam
  :commands (org-roam-buffer-toggle-display
             org-roam-dailies-goto-date
             org-roam-dailies-goto-today
             org-roam-dailies-goto-tomorrow
             org-roam-dailies-goto-yesterday)
  :after org org-capture
  :init
  (setopt
   org-roam-v2-ack t
   org-roam-directory org-default-folder
   org-roam-db-location (concat doom-local-dir "org-roam.db")
   org-roam-dailies-directory "daily/"

   ;; org-mode doesn't know how to properly export with roam links
   org-export-with-broken-links t
   org-roam-file-exclude-regexp '("data/" ".sync/"))
  :config
  (map! :map org-mode-map
        :i "[[" #'org-roam-node-insert+
        :i "[ SPC" (cmd! (insert "[]")
                         (backward-char)))
  (map! :map org-roam-mode-map
        "C-c i" #'org-roam-node-insert+
        :n "q" #'quit-window
        (:prefix ("g" . "goto")
                 "k" #'org-backward-element
                 "j" #'org-forward-element)
        (:localleader
         (:prefix ("r" . "roam")
                  "f" #'org-roam-node-find
                  "l" #'org-roam-buffer-toggle)))

  ;; always open Backlinks in other-window; or in the same window with universal arg
  (map! :map org-roam-preview-map
        "RET" (cmd! (let ((current-prefix-arg (if current-prefix-arg nil 2)))
                      (call-interactively #'org-roam-preview-visit))))
  (map! :map org-roam-mode-map
        "RET" (cmd! (let ((current-prefix-arg (if current-prefix-arg nil 2)))
                      (call-interactively #'org-roam-node-visit))))

  (after! xwidget
    (map! :localleader :map xwidget-webkit-mode-map
          (:prefix ("r" . "roam")
                   "w" #'org-roam-toggle-ui-xwidget)))

  (setopt
   org-roam-completion-everywhere nil
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setopt org-roam-capture-templates
          `(("d" "default" plain
             "%?"
             :if-new
             (file+head
              "${slug}.org"
              "\n#+title: ${title}\n#+startup: overview\n\n")
             :unnarrowed t
             :jump-to-captured t)))

  (setopt org-capture-templates
          `(("Q" "quote" entry
             (file ,(concat org-directory "quotes.org"))
             "* %c %?\n:PROPERTIES:\n:ID: %(org-id-new) \n:END:"
             :jump-to-captured t)
            ("p" "person" entry
             (file ,(concat org-directory "people.org"))
             "* %(+person-w-name-based-id)\n%?"
             :jump-to-captured t)
            ("c" "colleague" entry
             (file ,(concat org-directory "coworkers.org"))
             "* %(+person-w-name-based-id)\n%?"
             :jump-to-captured t)))

  (setopt org-roam-capture-ref-templates
          '(("r" "ref" plain "%?" :if-new
             (file+head
              (concat "${slug}.org" "#+title: ${title}\n"
                      "#+title: ${title}\n#+startup: overview\n\n"
                      "%(org-roam--link-to \"unread\")\n"
                      "%(org--insert-selection-dwim \"${body}\")"))
             :unnarrowed t
             :jump-to-captured t)
            ("c" "chat-gpt" plain "%?" :if-new
             (file+head "${slug}.org" "%(+chat-gpt-page-summary \"${ref}\" \"${title}\")")
             :unnarrowed t
             :jump-to-captured t)))

  (setopt org-roam-dailies-capture-templates
          '(("w" "work" plain
             "**** %?%(org-roam-capture-dailies--set-node-props \"work\")"
             :if-new
             (file+datetree
              "%<%Y-%m>-work-notes.org"
              'day)
             :jump-to-captured t
             :immediate-finish t
             :unnarrowed t)
            ("j" "journal" plain
             "%(org-roam-capture-dailies--set-node-props \"journal\")**** %?"
             :if-new
             (file+datetree
              "%<%Y-%m>-journal.org"
              'day)
             :jump-to-captured t
             :unnarrowed t)))

  (advice-add #'org-roam-dailies--capture :around #'org-roam-capture-dont-create-id-a)

  (advice-add #'consult-org-roam-backlinks :override #'consult-org-roam-backlinks*)

  (defadvice! org-property-lowecase-a (orig-fn pom prop value)
    :around #'org-entry-put
    (funcall orig-fn pom (downcase prop) value))

  (org-roam-db-autosync-mode +1)

  (add-to-list
   'display-buffer-alist
   '("\\*org-roam\\*"
     (display-buffer-in-quadrant)
     (direction . right)
     (window . root)))

  (add-to-list 'org-default-properties "roam_aliases")
  (add-to-list 'org-default-properties "roam_refs"))

(use-package! org-roam-protocol
  :after org-roam)

(use-package! org-roam-dailies
  :commands (org-roam-dailies-capture-date
             org-roam-dailies-goto-today))

(use-package! org-roam-ui
  :after org-roam
  :init
  (setq org-roam-ui-port 8088
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
            ;; :i Cup    (cmds! (org-at-table-p) #'+org/table-previous-row
            ;;                  #'org-up-element)
            :i Cdown  (cmds! (org-at-table-p) #'org-table-next-row
                             #'org-down-element)
            :ni CSright   #'org-shiftright
            :ni CSleft    #'org-shiftleft
            :ni CSup      #'org-shiftup
            :ni CSdown    #'org-shiftdown
            ;; more intuitive RET keybinds
            :n [return]   #'+org/dwim-at-point
            :n "RET"      #'+org/dwim-at-point
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
            :n "zi"  #'org-link-preview

            :map org-read-date-minibuffer-local-map
            Cleft    (cmd! (org-funcall-in-calendar '(calendar-backward-day 1)))
            Cright   (cmd! (org-funcall-in-calendar '(calendar-forward-day 1)))
            Cup      (cmd! (org-funcall-in-calendar '(calendar-backward-week 1)))
            Cdown    (cmd! (org-funcall-in-calendar '(calendar-forward-week 1)))
            CSleft   (cmd! (org-funcall-in-calendar '(calendar-backward-month 1)))
            CSright  (cmd! (org-funcall-in-calendar '(calendar-forward-month 1)))
            CSup     (cmd! (org-funcall-in-calendar '(calendar-backward-year 1)))
            CSdown   (cmd! (org-funcall-in-calendar '(calendar-forward-year 1)))))))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setopt org-appear-delay 1
          org-appear-autolinks t
          org-appear-autoemphasis t
          org-appear-autosubmarkers t
          ;; org-fold-core-style 'text-properties
          )

  ;; appear in evil normal state
  ;; (add-hook! 'org-mode-hook
  ;;   (defun enable-org-appear-in-insert-mode-h ()
  ;;     (setq org-appear-trigger 'manual)
  ;;     (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
  ;;     (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))
  )

(use-package! org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setopt org-superstar-leading-bullet ?\s
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
  :hook (org-mode . org-edit-indirect-mode)
  :config
  (setopt edit-indirect-guess-mode-function #'edit-indirect-guess-mode-fn+))

(use-package! consult-org-roam
  :after org-roam
  :config
  (setopt consult-org-oram-grep-func #'consult-ripgrep))

(use-package! ox-gfm
  :after org
  :config
  (setopt org-export-with-toc nil))

(use-package! org-pomodoro
  :after org
  :config
  (map! "C-x p p" #'org-pomodoro)
  (setopt org-pomodoro-start-sound-p t
          org-pomodoro-killed-sound-p t
          org-pomodoro-audio-player (format "%s -volume 50" (executable-find "mplayer"))
          org-pomodoro-start-sound "~/.doom.d/modules/custom/org/pomodoro__race-start.mp3"
          org-pomodoro-short-break-sound "~/.doom.d/modules/custom/org/pomodoro__break-over.mp3")

  (add-hook! '(org-clock-in-hook
               org-clock-out-hook
               org-pomodoro-break-finished-hook
               org-pomodoro-started-hook
               org-pomodoro-killed-hook
               org-pomodoro-finished-hook)
             #'menu-bar-item-set-clock-or-pomodoro))

(use-package! verb
  :after org
  :config
  (defvar verb-edn-request-enabled t)
  (defvar verb-edn-response-enabled t)

  (setopt verb-inhibit-cookies t   ; I'll handle them manually
          verb-json-use-mode 'json-mode)
  (map! :map org-mode-map
        (:localleader "v" verb-command-map
                      (:prefix ("v" . "verb")
                               "r" #'verb-send-request-on-point-other-window-stay)))

  (map! :map verb-response-body-mode-map
        :n "q" #'kill-buffer-and-window)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t)))

  (advice-add 'verb--request-spec-post-process :around #'verb--request-spec-post-process-a)
  (add-hook! 'verb-post-response-hook #'verb-post-response-h))

(use-package! ob-http
  :after org
  :commands org-babel-execute:http)


;; consult-line and consult-org-heading won't reveal the context
;; consult search commands won't reveal the context
;; see: minad/consult#563
(after! consult
  (defadvice! evil-ex-store-pattern-a (_ _)
    "Remember the search pattern after consult-line."
    :after #'consult-line
    (setopt evil-ex-search-pattern
            (list (car consult--line-history) t t)))

  (defadvice! org-show-entry-consult-a (fn &rest args)
    :around #'consult-line
    :around #'consult-org-heading
    :around #'consult--grep
    :around #'compile-goto-error
    (when-let ((pos (apply fn args)))
      (when (derived-mode-p 'org-mode)
        (org-fold-show-entry)))))

(use-package! anki-editor
  :commands (anki-editor-mode anki-editor-push-notes anki-editor-push-tree)
  :config
  (setopt anki-editor-create-decks t      ; Allow anki-editor to create a new deck if it doesn't exist
          anki-editor-org-tags-as-anki-tags t)

  (defvar anki-editor-mode-map (make-sparse-keymap))

  (map! :map anki-editor-mode-map
        :localleader
        (:prefix ("a" . "anki")
                 "p" #'anki-editor-push-tree))

  (add-to-list 'minor-mode-map-alist '(anki-editor-mode anki-editor-mode-map)))

(use-package! anki-editor-ui
  :after anki-editor)

(use-package! toc-org
  :after org
  ;; :hook (org-mode . toc-org-enable)
  :config
  (setopt toc-org-hrefify-default "gh"))

(use-package! org-modern-indent
  :hook (org-mode . org-modern-indent-mode))

;; (use-package! khoj
;;   :after (org org-roam)
;;   :config
;;   (setq
;;    khoj-index-directories (list org-default-folder)
;;    khoj-index-files nil
;;    khoj-server-url "http://127.0.0.1:42110")
;;   (add-hook! org-roam-mode #'khoj--server-start))

(use-package! ob-mermaid
  :defer t
  :after (org)
  :config
  ;; install https://github.com/mermaid-js/mermaid-cli
  (when-let* ((mmdc (executable-find "mmdc")))
    (setopt ob-mermaid-cli-path mmdc)))
