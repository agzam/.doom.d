;;; custom/clojure/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "project.clj" "build.boot" "deps.edn"))

(use-package! clojure-mode
  :config
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
      (add-to-list 'lsp-language-id-configuration (cons m "clojure")))))

(use-package! cider
  :init
  (after! clojure-mode
    (set-repl-handler! 'clojure-mode #'+clojure/open-repl :persist t)
    (set-repl-handler! 'clojurescript-mode #'+clojure/open-cljs-repl :persist t)
    (set-eval-handler! '(clojure-mode clojurescript-mode) #'cider-eval-region))

  :config
  (set-lookup-handlers! '(cider-mode cider-repl-mode)
    :definition #'+clojure-cider-lookup-definition
    :documentation #'cider-doc)

  (set-popup-rules!
   '(("^\\*cider-error*" :ignore t)
     ("^\\*cider-repl" :quit nil :ttl nil)
     ("^\\*cider-repl-history" :vslot 2 :ttl nil)))

  (setq nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-font-lock-dynamically '(macro core function var deprecated)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file (concat doom-cache-dir "cider-repl-history")
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-print-length 100
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-stacktrace-default-filters '(tooling dup)

        ;; Don't focus the CIDER REPL when it starts. Since it can take so long
        ;; to start up, you either wait for a minute doing nothing or be
        ;; prepared for your cursor to suddenly change buffers without warning.
        ;; See https://github.com/clojure-emacs/cider/issues/1872
        cider-repl-pop-to-buffer-on-connect 'display-only)

  ;; When in cider-debug-mode, override evil keys to not interfere with debug keys
  (after! evil
    (add-hook! cider--debug-mode
      (defun +clojure--cider-setup-debug ()
        "Setup cider debug to override evil keys cleanly"
        (evil-make-overriding-map cider--debug-mode-map 'normal)
        (evil-normalize-keymaps))))

  (after! ob-clojure
    (setq! org-babel-clojure-backend 'cider))

  (map! (:localleader
         (:map (clojure-mode-map clojurescript-mode-map)
          "'"  #'cider-jack-in-clj
          "\"" #'cider-jack-in-cljs
          "c"  #'cider-connect-clj
          "C"  #'cider-connect-cljs
          "m"  #'cider-macroexpand-1
          "M"  #'cider-macroexpand-all
          (:prefix ("d" . "debug")
           "d" #'cider-debug-defun-at-point)
          (:prefix ("e" . "eval")
           "b" #'cider-eval-buffer
           "d" #'cider-eval-defun-at-point
           "D" #'cider-insert-defun-in-repl
           "e" #'cider-eval-last-sexp
           "E" #'cider-insert-last-sexp-in-repl
           "r" #'cider-eval-region
           "R" #'cider-insert-region-in-repl
           "u" #'cider-undef)
          (:prefix ("g" . "goto")
           "b" #'cider-pop-back
           "g" #'cider-find-var
           "n" #'cider-find-ns)
          (:prefix ("h" . "help")
           "n" #'cider-find-ns
           "a" #'cider-apropos
           "c" #'cider-clojuredocs
           "d" #'cider-doc
           "j" #'cider-javadoc
           "w" #'cider-clojuredocs-web)
          (:prefix ("i" . "inspect")
           "e" #'cider-enlighten-mode
           "i" #'cider-inspect
           "r" #'cider-inspect-last-result)
          (:prefix ("n" . "namespace")
           "n" #'cider-browse-ns
           "N" #'cider-browse-ns-all
           "r" #'cider-ns-refresh)
          (:prefix ("p" . "print")
           "p" #'cider-pprint-eval-last-sexp
           "P" #'cider-pprint-eval-last-sexp-to-comment
           "d" #'cider-pprint-eval-defun-at-point
           "D" #'cider-pprint-eval-defun-to-comment
           "r" #'cider-pprint-eval-last-sexp-to-repl)
          (:prefix ("r" . "repl")
           "n" #'cider-repl-set-ns
           "q" #'cider-quit
           "r" #'cider-ns-refresh
           "R" #'cider-restart
           "b" #'cider-switch-to-repl-buffer
           "B" #'+clojure/cider-switch-to-repl-buffer-and-switch-ns
           "c" #'cider-find-and-clear-repl-output
           "l" #'cider-load-buffer
           "L" #'cider-load-buffer-and-switch-to-repl-buffer)
          (:prefix ("t" . "test")
           "a" #'cider-test-rerun-test
           "l" #'cider-test-run-loaded-tests
           "n" #'cider-test-run-ns-tests
           "p" #'cider-test-run-project-tests
           "r" #'cider-test-rerun-failed-tests
           "s" #'cider-test-run-ns-tests-with-filters
           "t" #'cider-test-run-test)))

        (:when (featurep! :editor evil +everywhere)
         :map cider-repl-mode-map
         :i [S-return] #'cider-repl-newline-and-indent
         :i [M-return] #'cider-repl-return
         (:localleader
          "n" #'cider-repl-set-ns
          "q" #'cider-quit
          "r" #'cider-ns-refresh
          "R" #'cider-restart
          "c" #'cider-repl-clear-buffer)
         :map cider-repl-history-mode-map
         :i [return]  #'cider-repl-history-insert-and-quit
         :i "q"  #'cider-repl-history-quit
         :i "l"  #'cider-repl-history-occur
         :i "s"  #'cider-repl-history-search-forward
         :i "r"  #'cider-repl-history-search-backward
         :i "U"  #'cider-repl-history-undo-other-window)))

(use-package! clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (set-lookup-handlers! 'clj-refactor-mode
    :references #'cljr-find-usages)
  (map! :map clojure-mode-map
        :localleader
        :desc "refactor" "R" #'hydra-cljr-help-menu/body))
