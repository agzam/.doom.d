;;; custom/clojure/autoload/tools.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cider-storm-storm-start-gui+ ()
  (interactive)
  (cider-storm--ensure-connected
   (cider-interactive-eval
    (format
     "((requiring-resolve 'flow-storm.api/local-connect) {:theme :%s :styles \"%s\"})"
     cider-storm-flow-storm-theme
     cider-storm-styles-path))))

;;;###autoload
(defun clj-dev-tool-profile+ ()
  (interactive)
  (unless (cider-connected-p)
    (user-error "CIDER not connected"))
  (let* ((current-ns (cider-current-ns))
         (form (save-excursion
                 (forward-char 1)
                 (cider-last-sexp)))
         (clj-cmd (format
                   (concat
                    "(do (require 'clj-async-profiler.core)"
                    "(clj-async-profiler.core/profile %s))")
                   form)))
    (cider-interactive-eval clj-cmd nil nil `(("ns" ,current-ns)))
    (run-with-timer
     0.5 nil ; half a second should be enough to write results to a file
     (lambda ()
       (let ((latest-file
              (thread-last
                (directory-files
                 "/tmp/clj-async-profiler/results/" t "\\.html\\'")
                (seq-sort
                 (lambda (a b)
                   (time-less-p (file-attribute-modification-time
                                 (file-attributes b))
                                (file-attribute-modification-time
                                 (file-attributes a)))))
                car)))
         (shell-command
          (format "open \"file:///%s\"" latest-file)))))))

;;;###autoload
(defun clj-dev-tool-bench+ ()
  (interactive)
  (let* ((current-ns (cider-current-ns))
	 (form (save-excursion
                 (forward-char 1)
                 (cider-last-sexp)))
	 (clj-cmd (format "(do (require 'criterium.core) (criterium.core/quick-bench %s))" form)))
    (cider-interactive-eval clj-cmd nil nil `(("ns" ,current-ns)))))

;;;###autoload
(defun cider-storm-switch-to-gui+ ()
  (interactive)
  ;; calling shell-command synchronously is very slow
  ;; this works much faster, but I need to suppress the output since I don't care about it
  (let ((display-buffer-alist
         (list (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))))
    (async-shell-command "hs -c 'hs.application.find(\"Flowstorm debugger\"):focus()'")))

;;;###autoload
(defun cider-load-alias+ ()
  "Prompt for aliases and load the selected one"
  (interactive)
  (unless (cider-connected-p)
    (user-error "CIDER not connected"))
  (let* ((clj (concat
               "(do (require '[clojure.tools.deps.cli.api :as deps.api]) "
               "(deps.api/aliases nil))"))
         (res (nrepl-dict-get (cider-sync-tooling-eval clj) "out"))
         (res (if (null res)
                  (shell-command-to-string
                   "clojure -X:deps aliases")
                res))
         (aliases (seq-filter
                   (-partial #'string-match-p "^:.*")
                   (string-lines res)))
         (prompt (format "Load alias in %s "
                         (cider-current-dir)))
         (choice (replace-regexp-in-string
                  " (.*)" ""
                  (completing-read prompt aliases nil t)))
         (clj (format "(sync-deps :aliases [%s])" choice)))
    (print
     (concat
      "loaded:\n"
      (nrepl-dict-get (cider-sync-tooling-eval clj) "value")))))

;;;###autoload
(defun cider-load-clojure-lib+ (&optional lib-coords)
  "Load a Clojure library into current cider repl session.
If LIB-COORDS string passed:
e.g., \"hiccup/hiccup {:mvn/version \"2.0.0-RC2\"}\"
it loads that, otherwise runs neil to choose a library."
  (interactive)
  (unless (cider-connected-p)
    (user-error "CIDER not connected"))
  (when-let* ((selected-lib (or lib-coords
                                (call-interactively #'neil-find-clojure-package)))
              (clj-form (format "(add-libs '{%s})"
                                selected-lib)))
    (print
     (concat
      "loaded: "
      (nrepl-dict-get
       (cider-sync-tooling-eval clj-form) "value"))
     nil)))
