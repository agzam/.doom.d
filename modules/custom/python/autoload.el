;;; custom/python/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mise-python-dir ()
  (thread-first
    (executable-find "mise")
    (concat " where python")
    shell-command-to-string
    string-trim))
