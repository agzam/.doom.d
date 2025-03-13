;;; custom/clojure/autoload/cider.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure-cider-lookup-definition (identifier)
  "A lookup handler for `cider-mode'.

This is necessary to fix `cider-find-dwim's inability to capture the full symbol
at point."
  (cider-find-dwim identifier))

;;;###autoload
(defun +clojure/open-repl (&optional arg type)
  "Open a Cider REPL for clojure and return the buffer."
  (interactive "P")
  ;; TODO Better error handling
  ;; type is `clj' for clojure and `cljs' for clojurescript
  ;; ... with no type specified, assume `clj'.
  (let ((type (or type 'clj)))
    (if-let (buffer (cider-current-repl type))
        (pop-to-buffer buffer)
      (let ((process (cond ((eq type 'clj) (cider-jack-in-clj arg))
                           ((eq type 'cljs) (cider-jack-in-cljs arg)))))
        (message "Starting CIDER server for the first time...")
        (while (and (process-live-p process)
                    (not (cider-current-repl type)))
          (sit-for 1))
        (message "Starting CIDER server for the first time...done")
        (pop-to-buffer (cider-current-repl type))))))


;;;###autoload
(defun +clojure/open-cljs-repl (&optional arg)
  "Open a Cider REPL for clojurescript and return the buffer."
  (interactive "P")
  (+clojure/open-repl arg 'cljs))

;;;###autoload
(defun +clojure/cider-switch-to-repl-buffer-and-switch-ns ()
  "TODO"
  (interactive)
  (cider-switch-to-repl-buffer t))

;;;###autoload
(defun cider-switch-to-nrepl-buffer ()
  "Calls cider-find-and-clear-repl-output interactively with C-u prefix
set so that it clears the whole REPL buffer, not just the output."
  (interactive)
  (when-let ((nrepl-buf (nrepl-make-buffer-name
                         (nrepl--make-hidden-name nrepl-server-buffer-name-template)
                         nil :no-dup)))
    (switch-to-buffer-other-window nrepl-buf)))

;;;###autoload
(defun cider-clear-repl-buffers ()
  "Clears both repl and nrepl output"
  (interactive)
  (let ((origin-buf (current-buffer))
        (orig-mode major-mode)
        (cider-buf (cider-current-repl nil 'ensure)))
    (switch-to-buffer cider-buf)
    (cider-repl-clear-buffer)
    (when (not (eq origin-buf cider-buf))
      (select-window (get-buffer-window origin-buf)))
    (when-let ((nrepl-buf (nrepl-make-buffer-name
                           (nrepl--make-hidden-name nrepl-server-buffer-name-template)
                           nil :no-dup)))
      (set-buffer nrepl-buf)
      (comint-clear-buffer))))

;;;###autoload
(defun cider-fqn-symbol-at-point ()
  "Return fully qualified symbol at point"
  (when (cider-connected-p)
    (let* ((ns (cider-current-ns))
           (_ (cider-sync-tooling-eval
               (format "(require '%s)" ns)))
           (sym (cider-symbol-at-point t))
           (res (cider-sync-tooling-eval
                 (format "`(%s)" sym) ns)))
      (if res
        (string-trim
         (replace-regexp-in-string
          "[()]" ""
          (nrepl-dict-get res "value")))
        (error "can't resolve ns")))))

;;;###autoload
(defun clj-fully-qualified-symbol-at-point (&optional for-req callback)
  "Gets fully qualified Clojure symbol at point. If FOR-REQ argument passed
gets the name suitable for :require of ns declaration."
  (interactive "P")
  (let* ((use-results (lambda (x)
                        (if callback
                            (funcall callback x)
                          (progn
                            (message x)
                            (kill-new x)
                            x))))
         (sym (cond ((cider-connected-p)
                     (cider-fqn-symbol-at-point))

                    ((lsp--capability :hoverProvider)
                     (when-let* ((cnt (-some->
                                          "textDocument/hover"
                                        (lsp--make-request
                                         (lsp--text-document-position-params))
                                        (lsp--send-request)
                                        (lsp:hover-contents)))
                                 ;; I think with native-comp it comes as a hash,
                                 ;; otherwise as cons
                                 (s (or (plist-get cnt :value)
                                        (gethash "value" cnt))))
                       (string-match "```clojure\n\\([[:graph:]]+/[[:graph:]]+\\)" s)
                       (string-trim (match-string 1 s))))

                    (t (message "Neither lsp nor cider are connected")))))
    (if for-req  ; want ns header name, e.g.: "[foo.core :as foo]"
        (if-let* ((m (string-match "^\\(.*\\)\\/" sym))) ; attempt to get anything before the slash
            (let* ((suffix (match-string 1 sym))  ; grab suffix of the symbol i.e. 'foo.core' of 'foo.core/my-thing'
                   ;; grep for '[foo.core :as ...' in the project
                   (grepped (string-trim
                             (shell-command-to-string
                              (format
                               "rg --glob '*.clj' --max-count 1 --no-filename '\\[%s :as' %s"
                               suffix
                               (projectile-project-root))))))
              (if-let* ((m (string-match "\\[.*\\]" grepped))
                        (res (match-string 0 grepped)))
                  (run-with-timer 0.04 nil use-results res)
                (run-with-timer 0.04 nil use-results (format "[%s :as ]" suffix))))
          (funcall use-results sym))
      (funcall use-results sym))))

;;;###autoload
(defun cider-hide-repl-buffers ()
  (interactive)
  (when-let (repl-buffer-win (get-buffer-window (cider-current-repl)))
    (quit-window nil repl-buffer-win))
  (when-let ((nrepl-buf-win (get-buffer-window
                             (nrepl-make-buffer-name
                              (nrepl--make-hidden-name nrepl-server-buffer-name-template)
                              nil :no-dup))))
    (quit-window nil nrepl-buf-win)))


;;;###autoload
(defun cider-test-run-focused-test ()
  "Run test around point."
  (interactive)
  (cider-load-buffer)
  (cider-test-run-test))

;;;###autoload
(defun +cider-test-result-buffer-quit ()
  "Quit test popup and immediately focus on the REPL."
  (interactive)
  (quit-restore-window (selected-window))
  (cider-switch-to-repl-buffer))

;;;###autoload
(defun cider-pprint-eval-sexp-at-point ()
  (interactive)
  (let ((evil-move-beyond-eol t))
    (save-excursion
      (when (looking-at "(\\|\\[\\|{")
        (forward-char))
      (when-let ((end-sexp (plist-get (sp-get-enclosing-sexp) :end)))
        (goto-char end-sexp))
      (call-interactively 'cider-pprint-eval-last-sexp))))

;;;###autoload
(defun kill-cider-buffers (&optional arg)
  "Kill all CIDER buffers in the project.
Won't ask any questions. Useful to execute when Emacs gets stuck.
With ARG, kills all buffers, not only in the current project"
  (interactive "P")
  (let ((blist
         (if arg
             (progn
               (message "killing all CIDER REPLs")
               (seq-filter
                (lambda (e) (string-match "\\*cider\\|\\*nrepl" (buffer-name e)))
                (buffer-list)))
           (when-let ((rs (cider-repls)))
             (message "killing REPLs for %s" (cider-current-dir))
             (append rs (list (get-buffer
                               (nrepl-make-buffer-name
                                (nrepl--make-hidden-name
                                 nrepl-server-buffer-name-template)
                                nil :no-dup)))))))
        (kill-buffer-query-functions nil))
    (thread-last
      blist
      (seq-map #'get-buffer-window)
      (seq-remove #'null)
      (seq-do (lambda (w) (quit-window :kill w))))
    (seq-do #'kill-buffer blist)))


;;;###autoload
(defun +cider-complete-at-point ()
  (when (cider-connected-p)
    (cider-complete-at-point)))

;;;###autoload
(defun cider-eval-sexp-at-point* ()
  (interactive)
  (if (looking-at "[])}]")
      (progn
        (forward-char)
        (cider-eval-last-sexp))
    (progn
      (unless (looking-at "[[({]")
        (sp-beginning-of-sexp)
        (backward-char))
      (cider-eval-sexp-at-point))))

;;;###autoload
(defun clojure-set-completion-at-point-h ()
  (setq-local completion-styles '(orderless
                                  partial-completion
                                  cider))

  (defalias 'cape-cider-lsp-yas
    (cape-capf-super #'+cider-complete-at-point
                     #'+lsp-completion-at-point
                     #'yasnippet-capf))

  (add-to-list 'completion-at-point-functions #'cape-cider-lsp-yas)
  (setq-local completion-at-point-functions
              (seq-difference
               completion-at-point-functions
               '(lsp-completion-at-point
                 cider-complete-at-point
                 yasnippet-capf)))
  (cape-completion-at-point-functions-h))

;;;###autoload
(defun clojure-project-root-path+ (&optional dir-name)
  "Extended version of clojure-project-root-path.
by default cider-jack-in always grabs firs .deps.edn it finds,
which isn't great for Polylith projects. Let's fix that."
  (if-let ((poly-root (locate-dominating-file "." "workspace.edn")))
      (clojure-project-root-path poly-root)
    (clojure-project-root-path dir-name)))


;;;###autoload
(defun org-edit-special-for-clojure-a (ofn &optional arg)
  "Advising function for editing clojure blocks that respect :nrepl-host header."
  (let ((find-matching-session
         (lambda (host port sessions)
           (let ((rx (format ".*\\:%s\\:%s" host port)))
             (seq-find (lambda (x)
                         (string-match-p rx (car x)))
                       sessions)))))
    (if-let* ((el (org-element-at-point))
              (src-p (eq 'src-block (org-element-type (org-element-context el))))
              (clj-p (string= "clojure" (org-element-property :language el)))
              (nrepl (alist-get
                      :nrepl-host
                      (nth 2 (org-babel-get-src-block-info))))
              (_ (string-match "\\(.*\\):\\([0-9]+\\)" nrepl))
              (host (match-string 1 nrepl))
              (port (match-string 2 nrepl)))
        (progn
          (funcall ofn arg)
          (if-let* ((ses (funcall find-matching-session
                                  host port
                                  (sesman-sessions (sesman--system)))))
              (sesman-link-with-buffer nil ses)
            (cider-connect-clj (list :host host
                                     :port (string-to-number port)))))
      (funcall ofn arg))))
