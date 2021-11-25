;;; custom/clojure/config.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "project.clj" "build.boot" "deps.edn"))

(use-package! clojure-mode
  :defer t
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
    :documentation #'cider-doc)

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
        cider-repl-pop-to-buffer-on-connect 'display-only)

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

  (map! (:map cider-eval-commands-map
         "C-c C-f" nil
         "C-c r" nil)
        (:map cider-mode-map
         "C-c C-f" nil
         "C-c r" nil))

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
     (window-width . 0.25)))

  ;; When in cider-debug-mode, override evil keys to not interfere with debug keys
  (after! evil
    (add-hook! 'cider--debug-mode-hook
      (defun +clojure--cider-setup-debug ()
        "Setup cider debug to override evil keys cleanly"
        (evil-make-overriding-map cider--debug-mode-map 'normal)
        (evil-normalize-keymaps))))

  (after! ob-clojure
    (setq! org-babel-clojure-backend 'cider))

  (map! (:localleader
         (:map (clojure-mode-map clojurescript-mode-map)
          ","  #'clj-fully-qualified-symbol-at-point
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
           "k" #'cider-hide-repl-buffers)
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
          (:prefix "s"
           "s" #'cider-switch-to-last-clojure-buffer
           "S" #'cider-switch-to-nrepl-buffer
           "l" #'cider-clear-repl-buffers
           "k" #'cider-hide-repl-buffers))
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
  (map! :map clojure-mode-map
        :localleader
        :desc "refactor" "R" #'hydra-cljr-help-menu/body))

(use-package! clojars
  :after clojure-mode)
