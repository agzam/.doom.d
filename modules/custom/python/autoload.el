;;; custom/python/autoload.el -*- lexical-binding: t; -*-

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
