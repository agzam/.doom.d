;;; custom/python/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python-mode-lookup-handlers ()
  (set-lookup-handlers! '(python-ts-mode
                          python-mode)
    :definition #'+lsp-lookup-definition-handler
    :references #'+lsp-lookup-references-handler
    :implementations '(lsp-find-implementation :async t)
    :type-definition #'lsp-find-type-definition
    :documentation #'+consult-dash-doc))

;;;###autoload
(defun python-insert-ipdb ()
  "Insert an ipdb breakpoint."
  (interactive)
  (python-indent-line)
  (insert "import ipdb; ipdb.set_trace()\n")
  (python-indent-line)
  ;; When you hit 'n' (next) in the debugger, it might skip over the
  ;; next real line of code or behave unexpectedly, especially if the
  ;; next line is a control flow statement (if, for, return, etc.)
  ;; So, we need some dummy statement here
  (insert "pass  # debugger\n")
  (python-indent-line))

;;;###autoload
(defun python-edit-imports ()
  "Edit python module header in an indirect buffer."
  (interactive)
  (save-mark-and-excursion
    (goto-char (point-min))
    ;; Skip shebang, encoding declarations, and module docstring
    (while (or (looking-at "^#")
               (looking-at "^\"\"\"")
               (looking-at "^'''"))
      (forward-line))
    (let ((start (point))
          (end (point)))
      ;; Find the end of the import block
      (while (and (not (eobp))
                  (or (looking-at "^import ")
                      (looking-at "^from ")
                      (looking-at "^[ \t]*$")  ; blank lines
                      (looking-at "^[ \t]*#"))) ; comments
        (forward-line)
        (unless (looking-at "^[ \t]*\\(#\\|$\\)")  ; Update end unless it's just comment/blank
          (when (looking-at "^\\(import\\|from\\) ")
            (setq end (line-end-position)))))
      ;; If no imports found, position at a reasonable place
      (when (= start end)
        (goto-char start)
        (setq end (point)))
      (let ((buf (edit-indirect-region start (1+ end) :display-buffer)))
        (with-current-buffer buf
          (python-mode)
          (goto-char (point-max))
          (evil-insert-state)
          (keymap-local-set "C-c C-k" #'edit-indirect-abort)
          (keymap-local-set "C-c C-c" #'edit-indirect-commit))))))

;;;###autoload
(defun python-to-json ()
  "Convert Python dict in region or buffer to JSON."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (shell-command-on-region 
     start end 
     "python -c \"import sys, json, ast; print(json.dumps(ast.literal_eval(sys.stdin.read()), indent=2))\""
     t t)))

;;;###autoload
(defun python-to-edn ()
  "Convert Python dict in region or buffer to EDN using jet."
  (interactive)
  (unless (executable-find "jet")
    (user-error "jet not found"))
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (shell-command-on-region 
     start end 
     "python -c \"import sys, json, ast; print(json.dumps(ast.literal_eval(sys.stdin.read())))\" | jet --from json --to edn --keywordize :kebab-case --no-commas"
     t t)))

;;;###autoload
(defun python-format (&optional ruff-cmd)
  "Format Python buffer/region with lsp or ruff."
  (interactive)
  (cond
   ((and lsp-mode (use-region-p)
         (lsp-feature? "textDocument/rangeFormatting"))
    (lsp-format-region))
   ((and lsp-mode (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer))
   (t (let* ((ruff-cmd (or ruff-cmd "uvx ruff format -"))
             (beg (if (use-region-p) (region-beginning) (point-min)))
             (end (if (use-region-p) (region-end) (point-max)))
             (content (buffer-substring beg end))
             (out (with-temp-buffer
                    (insert content)
                    (let* ((err (shell-command-on-region
                                 (point-min) (point-max) ruff-cmd nil t))
                           (out (buffer-substring (point-min) (point-max))))
                      (if (zerop err)
                          out (error "Can't format: %s" out))))))
        (delete-region beg end)
        (insert out)))))

(defadvice! python--fix-imports-with-ruff-a (&optional _beg _end)
  "Use Ruff instead of pyflakes."
  :override #'python-fix-imports
  (save-buffer)
  (shell-command (concat "ruff check --select F401 --fix " (buffer-file-name)))
  (revert-buffer t t t))

;;; FQN resolution

(defun python--path-to-dotted (rel-path)
  "Convert \"foo/bar/__init__\" to \"foo.bar\"."
  (thread-last rel-path
    (replace-regexp-in-string "/" ".")
    (replace-regexp-in-string "\\.__init__$" "")))

(defun python--resolve-project-module (filepath)
  "Derive Python module from FILEPATH by walking up __init__.py chain."
  (let* ((dir (file-name-directory filepath))
         (base (file-name-sans-extension (file-name-nondirectory filepath)))
         (parts (if (string= base "__init__") '() (list base))))
    (while (and dir (file-exists-p (expand-file-name "__init__.py" dir)))
      (push (file-name-nondirectory (directory-file-name dir)) parts)
      (setq dir (file-name-directory (directory-file-name dir))))
    (when parts (string-join parts "."))))

(defun python--uri-to-module (uri)
  "Derive Python module from a file URI (for LSP definition results)."
  (when-let* ((path (lsp--uri-to-path uri)))
    (cond
     ((string-match ".*/lib/python[0-9.]+/\\(.*\\)\\.pyi?$" path)
      (python--path-to-dotted (match-string 1 path)))
     ((string-match ".*/typestubs[^/]*/stdlib/\\(.*\\)\\.pyi?$" path)
      (python--path-to-dotted (match-string 1 path)))
     ((string-match ".*/site-packages/\\(.*\\)\\.pyi?$" path)
      (python--path-to-dotted (match-string 1 path)))
     ((string-match "\\.pyi?$" path)
      (python--resolve-project-module path)))))

(defun python--current-module ()
  "Python module path for the current buffer."
  (when-let* ((f (buffer-file-name)))
    (python--resolve-project-module f)))

(defun python--current-package ()
  "Current Python package (for resolving relative imports)."
  (when-let* ((module (python--current-module)))
    (if (string-match-p "/__init__\\.py$" (buffer-file-name))
        module
      (if (string-match "\\(.*\\)\\.[^.]+$" module)
          (match-string 1 module)
        ""))))

(defun python--resolve-module-ref (ref)
  "Resolve module REF, handling relative imports like \".config\"."
  (if (string-match "^\\(\\.+\\)\\(.*\\)" ref)
      (let* ((dots (length (match-string 1 ref)))
             (suffix (match-string 2 ref))
             (pkg (python--current-package))
             (pkg-parts (if (string-empty-p pkg) nil (split-string pkg "\\.")))
             (base (butlast pkg-parts (1- dots))))
        (string-join (append base (when (< 0 (length suffix)) (list suffix))) "."))
    ref))

(defun python--treesit-enclosing-scope ()
  "Walk up AST collecting enclosing class/function names (outermost first)."
  (let ((node (treesit-node-at (point)))
        parts)
    (while node
      (when (member (treesit-node-type node) '("class_definition" "function_definition"))
        (when-let* ((n (treesit-node-child-by-field-name node "name")))
          (push (treesit-node-text n t) parts)))
      (setq node (treesit-node-parent node)))
    parts))

(defun python--treesit-resolve-import (sym)
  "Resolve SYM through import statements using treesit."
  (when-let* ((_ (treesit-parser-list))
              (root (treesit-buffer-root-node)))
    (or
     ;; from X import sym  /  from X import Y as sym
     (cl-loop
      for (_ . node) in (treesit-query-capture root '((import_from_statement) @imp))
      for mod-node = (treesit-node-child-by-field-name node "module_name")
      for mod-text = (when mod-node (treesit-node-text mod-node t))
      thereis
      (cl-loop
       for i from 0 below (treesit-node-child-count node)
       for child = (treesit-node-child node i)
       when (equal (treesit-node-field-name child) "name")
       thereis
       (pcase (treesit-node-type child)
         ("dotted_name"
          (when (equal (treesit-node-text child t) sym)
            (format "%s.%s" (python--resolve-module-ref mod-text) sym)))
         ("aliased_import"
          (when-let* ((alias (treesit-node-child-by-field-name child "alias"))
                      (_ (equal (treesit-node-text alias t) sym))
                      (orig (treesit-node-child-by-field-name child "name")))
            (format "%s.%s" (python--resolve-module-ref mod-text)
                    (treesit-node-text orig t)))))))
     ;; import sym  /  import X as sym
     (cl-loop
      for (_ . node) in (treesit-query-capture root '((import_statement) @imp))
      thereis
      (cl-loop
       for i from 0 below (treesit-node-child-count node)
       for child = (treesit-node-child node i)
       when (equal (treesit-node-field-name child) "name")
       thereis
       (pcase (treesit-node-type child)
         ("dotted_name"
          (when (equal (treesit-node-text child t) sym) sym))
         ("aliased_import"
          (when-let* ((alias (treesit-node-child-by-field-name child "alias"))
                      (_ (equal (treesit-node-text alias t) sym))
                      (orig (treesit-node-child-by-field-name child "name")))
            (treesit-node-text orig t)))))))))

(defun python--fqn-via-lsp ()
  "Resolve FQN via LSP definition location + hover."
  (condition-case nil
      (let* ((sym (thing-at-point 'symbol t))
             (defs (lsp-request "textDocument/definition"
                                (lsp--text-document-position-params)))
             (def-uri (when (car defs) (lsp-get (car defs) :uri)))
             (module (when def-uri (python--uri-to-module def-uri)))
             (hover (when-let* ((cnt (-some-> "textDocument/hover"
                                       (lsp--make-request (lsp--text-document-position-params))
                                       (lsp--send-request)
                                       (lsp:hover-contents))))
                      (or (plist-get cnt :value) (gethash "value" cnt))))
             (class-name (when (and hover (string-match
                                           "Self@\\([a-zA-Z_][a-zA-Z0-9_]*\\)" hover))
                           (match-string 1 hover))))
        (string-join (delq nil (list module class-name sym)) "."))
    (error nil)))

;;;###autoload
(defun py-fully-qualified-symbol-at-point (&optional callback)
  "Get the fully qualified name of the Python symbol at point.
Copies to kill ring and messages the result.
With CALLBACK, calls it with the FQN string instead."
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
         (has-treesit (and (fboundp 'treesit-parser-list) (treesit-parser-list)))
         (fqn
          (or
           ;; 1. Treesit: at a definition name -> module + scope chain
           (when has-treesit
             (let* ((node (treesit-node-at (point)))
                    (parent (treesit-node-parent node)))
               (when (and parent
                          (member (treesit-node-type parent)
                                  '("function_definition" "class_definition"))
                          (equal (treesit-node-field-name node) "name"))
                 (when-let* ((scope (python--treesit-enclosing-scope))
                             (module (python--current-module)))
                   (string-join (cons module scope) ".")))))
           ;; 2. Treesit: resolve through imports
           (when (and has-treesit sym)
             (python--treesit-resolve-import sym))
           ;; 3. LSP fallback
           (when (and sym (bound-and-true-p lsp-mode) (lsp-workspaces))
             (python--fqn-via-lsp)))))
    (if fqn
        (if callback (funcall callback fqn)
          (message fqn)
          (kill-new fqn)
          fqn)
      (message "Could not resolve FQN")
      nil)))

;;;###autoload
(defun python-fix-all ()
  "Use Ruff to fix Python issues.

  - Unused imports (F401)
  - Unused variables (F841)
  - Undefined names (F821)
  - Whitespace issues
  - And other rules depending on your .ruff.toml or pyproject.toml"
  (interactive)
  (save-buffer)
  (shell-command (concat "ruff check --select F401 --fix " (buffer-file-name)))
  (revert-buffer t t t))
