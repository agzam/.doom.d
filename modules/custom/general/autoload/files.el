;;; custom/general/autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun insert-file-path (arg)
  "Prompt for a file path and insert it at point.
Without prefix ARG, insert the absolute path.
With prefix ARG, insert path relative to `default-directory'.
Use `consult-dir' (C-x C-d) during the prompt for zoxide lookup."
  (interactive "P")
  (let ((path (read-file-name "Insert path: ")))
    (insert (if arg
                (file-relative-name path)
              (abbreviate-file-name path)))))
