;;; custom/clojure/autoload/clojure.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure-mode-lookup-handlers ()
  (set-lookup-handlers! '(clojure-mode
                          clojurec-mode
                          clojurescript-mode
                          cider-clojure-interaction-mode
                          cider-repl-mode

                          clojure-ts-mode
                          clojure-ts-clojurec-mode
                          clojure-ts-clojurescript-mode)
    :definition #'+lsp-lookup-definition-handler
    :references #'+lsp-lookup-references-handler
    :implementations '(lsp-find-implementation :async t)
    :type-definition #'lsp-find-type-definition
    :documentation #'+consult-dash-doc))


;;;###autoload
(defun clojure-unalign (beg end)
  "Un-align (remove extra spaces) in vertically aligned sexp around the point."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (clojure-backward-logical-sexp)
                     (list (point) end)))))

  (save-mark-and-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " "))
      (let ((clojure-align-forms-automatically nil))
        (if lsp-mode
            (lsp--indent-lines beg end)
          (indent-region beg end))))))

;;;###autoload
(defun edn-string->json (edn-str)
  "Attempts to convert edn string to json using jet."
  (shell-command-to-string
   (concat "echo '" edn-str "' | "
           "jet --from edn --to json")))

;;;###autoload
(defun clojure-edn-json-transform (&optional from-json)
  "Transforms EDN to JSON and vice-versa using jet cli.
The direction is determined by current major-mode or can be
explicitly set by universal argument, if present - attemps to
convert from JSON."
  (interactive "P")
  (let* ((from-json* (or from-json (eq major-mode 'json-mode)))
         (region (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (save-excursion
                     (when (looking-at "\{\\|\\[")
                       (forward-char))
                     (let ((end (progn (sp-end-of-sexp)
                                       (point))))
                       (sp-beginning-of-sexp)
                       (backward-char)
                       (list (point) (+ 1 end))))))
         (jet (executable-find "jet"))
         (params (if from-json*
                     '("--from" "json" "--to" "edn"
                       "--keywordize"
                       "#(cond-> % (not (clojure.string/starts-with? % \"@\")) (keyword))"
                       "--pretty")
                   '("--from" "edn" "--to" "json" "--pretty"))))
    (when (not jet)
      (error "jet cli not found"))
    (save-excursion
      (apply 'call-process-region
             (car region)
             (cadr region)
             jet
             :delete
             '(t nil)
             :display
             params)
      ;; (sp-reindent) ; takes too long, trying to disable it
      )))

;;;###autoload
(defun separedit--remove-clj-str-delimeters (_ &optional _)
  (save-excursion
    (replace-regexp-in-region "^\s*\"" "" (point-min))
    (replace-regexp-in-region "\"\s*$" "" (point-min))))

;;;###autoload
(defun separedit--restore-clj-str-delimeters (&optional _)
  (save-excursion
    (replace-regexp-in-region "^\\s-*$" "\\\\n" (point-min))
    (replace-regexp-in-region "^" "\"" (point-min))
    (replace-regexp-in-region ".$" "\\& \"" (point-min))))


;;;###autoload
(defun clj-edit-ns-header ()
  (interactive)
  (save-mark-and-excursion
    (let ((edit-indirect-guess-mode-function (lambda (buf b_ e_)
                                               (funcall (buffer-local-value 'major-mode buf)))))
      (cljr--goto-ns)
      (sp-select-next-thing)
      (map! :map cider-mode-map "C-c C-k" nil)
      (let ((buf (edit-indirect-region (region-beginning) (region-end) :display-buffer)))
        (with-current-buffer buf
          (use-local-map cider-mode-map)
          (search-backward ":require")
          (sp-end-of-sexp)
          (newline-and-indent)
          (evil-insert-state)
          (keymap-local-set "C-c C-k" #'edit-indirect-abort)
          (keymap-local-set "C-c C-c" #'edit-indirect-commit))))))


;;;###autoload
(defun add-edn-imenu-regexp-h ()
  "Hacky way to get imenu for root-level keywords. Useful in edn files."
  (when (string= "edn" (file-name-extension (or (buffer-file-name) "")))
    (add-to-list 'imenu-generic-expression '(nil "^.?.?\\(:[^ ]+\\).*$" 1) t)))


;;;###autoload
(defun clj-fully-qualified-symbol-with-gh-link (&optional main-branch?)
  "Returns a markdown link to line number on GH with a Symbol Name"
  (interactive "P")
  (let* ((git-link-default-branch (when main-branch? (magit-main-branch)))
         (url (url-unhex-string (git-link-kill)))
         (symbol (let ((inhibit-message t))
                   (clj-fully-qualified-symbol-at-point))))
    (let ((inhibit-message t))
      (clj-fully-qualified-symbol-at-point
       nil
       (lambda (sym)
         (let ((link (format "[%s](%s)" sym url)))
           (message link)
           (kill-new link)
           link))))))
