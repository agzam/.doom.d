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
