;;; custom/clojure/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "project.clj" "build.boot" "deps.edn"))

(use-package! clojure-mode
  :defer t
  :config
  (add-to-list
   '+lookup-provider-url-alist
   '("Clojure Docs" "https://clojuredocs.org/search?q=%s"))

  (add-hook! '(clojure-mode-local-vars-hook
               clojurec-mode-local-vars-hook
               clojurescript-mode-local-vars-hook)
    (defun +clojure-disable-lsp-indentation-h ()
      (setq-local lsp-enable-indentation nil))
    #'lsp!)

  (after! lsp-clojure
    (dolist (m '(clojure-mode
                 clojurec-mode
                 clojurescript-mode
                 clojurex-mode))
      (add-to-list 'lsp-language-id-configuration (cons m "clojure"))))

  (add-hook! 'clojurescript-mode-hook
    (defun add-reframe-regs-to-imenu ()
      (add-to-list
       'imenu-generic-expression
       '("re-frame" "^(*reg-\\(event-db\\|sub\\|sub-raw\\|fx\\|event-fx\\|event-ctx\\|cofx\\)[ \n]+\\([^\t \n]+\\)" 2)
       t))))

(use-package! cider
  :after clojure-mode
  :hook (clojure-mode-local-vars . cider-mode)
  :init
  (after! clojure-mode
    (set-repl-handler! 'clojure-mode #'+clojure/open-repl :persist t)
    (set-repl-handler! 'clojurescript-mode #'+clojure/open-cljs-repl :persist t)
    (set-eval-handler! '(clojure-mode clojurescript-mode) #'cider-eval-region))

  :config
  (set-lookup-handlers! '(cider-mode cider-repl-mode)
    :definition #'+clojure-cider-lookup-definition
    :documentation #'cider-clojuredocs)

  ;; remove lsp's doc lookup handler, so it always uses cider-clojuredocs
  (add-hook! 'cider-mode-hook
    ;; don't let the name decieve you, works for corfu too
    (defun cider-completion-styles-h ()
      (setq-local
       completion-styles
       '(cider orderless partial-completion))
      (when (featurep 'cape)
        (cape-completion-at-point-functions-h))))

  (add-hook!
   'cider-repl-mode-hook
   ;; don't let the name decieve you, works for corfu too
   #'cider-company-enable-fuzzy-completion
   #'hs-minor-mode)

  (set-popup-rules!
   '(("^\\*cider-error*" :ignore t)
     ("^\\*cider-repl" :quit nil :ttl nil)
     ("^\\*cider-repl-history" :vslot 2 :ttl nil)))

  (setq nrepl-hide-special-buffers nil
        nrepl-log-messages nil
        ;; cider-font-lock-dynamically '(macro core function var deprecated)
        cider-font-lock-dynamically nil
        cider-dynamic-indentation nil
        cider-overlays-use-font-lock nil
        cider-prompt-for-symbol nil

        cider-repl-display-help-banner nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file (concat doom-cache-dir "cider-repl-history")
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-print-length 100
        cider-repl-use-clojure-font-lock nil
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-stacktrace-default-filters '(tooling dup)

        ;; Don't focus the CIDER REPL when it starts. Since it can take so long
        ;; to start up, you either wait for a minute doing nothing or be
        ;; prepared for your cursor to suddenly change buffers without warning.
        ;; See https://github.com/clojure-emacs/cider/issues/1872
        cider-repl-pop-to-buffer-on-connect 'display-only

        cider-test-show-report-on-success t)

  (setq
   clojure-enable-fancify-symbols nil
   clojure-align-forms-automatically nil
   cljr-favor-prefix-notation nil
   cider-repl-display-in-current-window nil
   cider-comment-prefix  " \n;; => "
   cider-inject-dependencies-at-jack-in t
   cider-repl-display-in-current-window nil
   cider-eldoc-display-for-symbol-at-point nil
   cider-eldoc-display-context-dependent-info nil)

  (after! cider-scratch
    (setq cider-scratch-buffer-name "*clj-scratch*"))

  (map! (:map cider-eval-commands-map
              "C-c C-f" nil
              "C-c r" nil)
        (:map cider-mode-map
              "C-c C-f" nil
              "C-c r" nil
              "C-c C-n" #'clj-edit-ns-header))

  (map! :map cider-popup-buffer-mode-map
        :n "q" #'cider-popup-buffer-quit-function)

  (map! :map cider-test-report-mode-map
        :n "q" #'+cider-test-result-buffer-quit)

  (add-to-list
   'display-buffer-alist
   `(,(rx bos (or "*cider-repl"
                  "*nrepl-server"
                  "*cider-test-report*"
                  "*cider-error"
                  "*cider-result"))
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (dedicated . nil)
     (window-width . 0.30)))

  ;; When in cider-debug-mode, override evil keys to not interfere with debug keys
  (after! evil
    (add-hook! 'cider--debug-mode-hook
      (defun +clojure--cider-setup-debug ()
        "Setup cider debug to override evil keys cleanly"
        (evil-make-overriding-map cider--debug-mode-map 'normal)
        (evil-normalize-keymaps)
        ;; liberate evil keys
        (map! :map cider--debug-mode-map
              :nv "h" nil :n "j" nil :n "l" nil :n "o" nil :n "p" nil :n "i" nil)))

    (advice-add 'cider-eval-sexp-at-point :around #'cider-eval-sexp-at-point-a)
    (advice-add 'cider-pprint-eval-last-sexp-to-comment :around #'cider-eval-sexp-at-point-a))

  (after! ob-clojure
    (setq! org-babel-clojure-backend 'cider))

  (after! edit-indirect
    (add-hook! 'edit-indirect-before-commit-hook
      (defun clj-sort-ns-after-edit-ns-header-h ()
        (when (string-match-p "clojure" (format "%s" major-mode))
          ;; fix dangling paren
          (goto-char (point-max))
          (search-backward ":require" nil :noerror)
          (sp-end-of-sexp)
          (let ((pos (point)))
            (search-backward "]" nil :noerror)
            (funcall-interactively
             #'replace-regexp
             "\n\\| " "" nil (point) pos))
          (sp-reindent)
          (clojure-sort-ns)))))

  (map! (:localleader
         (:map (clojure-mode-map clojurescript-mode-map)
               ","  #'clj-fully-qualified-symbol-at-point
               ";" #'clojure-toggle-ignore
               "m"  #'cider-macroexpand-1
               "M"  #'cider-macroexpand-all
               (:prefix ("d" . "debug")
                        "f" #'cider-debug-defun-at-point)
               (:prefix ("f" . "format")
                        "l" #'clojure-align
                        "L" #'clojure-unalign)
               (:prefix ("e" . "eval")
                        "b" #'cider-eval-buffer
                        "c" #'cider-eval-sexp-at-point
                        "d" #'cider-eval-defun-at-point
                        "D" #'cider-insert-defun-in-repl
                        "p" #'cider-pprint-eval-sexp-at-point
                        ";" #'cider-pprint-eval-last-sexp-to-comment
                        "e" #'cider-eval-last-sexp
                        "E" #'cider-insert-last-sexp-in-repl
                        "r" #'cider-eval-region
                        "R" #'cider-insert-region-in-repl
                        "u" #'cider-undef)
               (:prefix ("g" . "goto")
                        "b" #'cider-pop-back
                        "g" #'cider-find-var
                        "n" #'cider-find-ns
                        "s" #'cider-scratch)
               (:prefix ("h" . "help")
                        "n" #'cider-find-ns
                        "a" #'cider-apropos
                        "c" #'cider-clojuredocs
                        "d" #'cider-doc
                        "j" #'cider-javadoc
                        "w" #'cider-clojuredocs-web)
               (:prefix ("i" . "inspect")
                        "e" #'cider-enlighten-mode
                        "i" #'clj-fully-qualified-symbol-at-point
                        "I" #'cider-inspect
                        "r" #'cider-inspect-last-result)
               (:prefix ("j" . "jack-in")
                        "j" #'cider-jack-in
                        "s" #'cider-jack-in-cljs
                        "a" #'cider-jack-in-clj&cljs
                        "c"  #'cider-connect-clj
                        "C"  #'cider-connect-cljs)
               (:prefix ("n" . "namespace")
                        "e" #'clj-edit-ns-header
                        "n" #'cider-browse-ns
                        "N" #'cider-browse-ns-all
                        "r" #'cider-ns-refresh)
               (:prefix ("p" . "print")
                        "p" #'cider-pprint-eval-sexp-at-point
                        "P" #'cider-pprint-eval-last-sexp-to-comment
                        "d" #'cider-pprint-eval-defun-at-point
                        "D" #'cider-pprint-eval-defun-to-comment
                        "r" #'cider-pprint-eval-last-sexp-to-repl)
               (:prefix ("s" . "repl")
                        "s" #'cider-switch-to-repl-buffer
                        "S" #'cider-switch-to-nrepl-buffer
                        "l" #'cider-clear-repl-buffers
                        "n" #'cider-repl-set-ns
                        "q" #'cider-quit
                        "r" #'cider-ns-refresh
                        "R" #'cider-restart
                        "B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
                        "k" #'cider-hide-repl-buffers
                        "K" #'kill-cider-buffers)
               (:prefix ("t" . "test")
                        "a" #'cider-test-rerun-test
                        "l" #'cider-test-run-loaded-tests
                        "n" #'cider-test-run-ns-tests
                        "p" #'cider-test-run-project-tests
                        "r" #'cider-test-rerun-failed-tests
                        "s" #'cider-test-run-ns-tests-with-filters
                        "t" #'cider-test-run-focused-test)))

        (:map cider-repl-mode-map
         :i [S-return] #'cider-repl-newline-and-indent
         :i [M-return] #'cider-repl-return
         (:localleader
          "n" #'cider-repl-set-ns
          "q" #'cider-quit
          "r" #'cider-ns-refresh
          "R" #'cider-restart
          "c" #'cider-repl-clear-buffer
          (:prefix ("s" . "repl")
                   "s" #'cider-switch-to-last-clojure-buffer
                   "S" #'cider-switch-to-nrepl-buffer
                   "l" #'cider-clear-repl-buffers
                   "k" #'cider-hide-repl-buffers
                   "K" #'kill-cider-buffers))
         :map cider-repl-history-mode-map
         :i [return]  #'cider-repl-history-insert-and-quit
         :i "q"  #'cider-repl-history-quit
         :i "l"  #'cider-repl-history-occur
         :i "s"  #'cider-repl-history-search-forward
         :i "r"  #'cider-repl-history-search-backward
         :i "U"  #'cider-repl-history-undo-other-window)))

(use-package! clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (set-lookup-handlers! 'clj-refactor-mode
    :references #'cljr-find-usages)
  (setq cljr-magic-requires nil)
  (map! :map clojure-mode-map
        :localleader
        :desc "refactor" "R" #'hydra-cljr-help-menu/body))

(use-package! clojars
  :after clojure-mode)

(use-package! neil
  :commands neil-find-clojure-package
  :config
  (setq neil-prompt-for-version-p nil))

(use-package! fennel-mode
  :mode "\\.fnl$"
  :defer t)
