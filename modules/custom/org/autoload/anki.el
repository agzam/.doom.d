;;; custom/org/autoload/anki.el -*- lexical-binding: t; -*-

;;;###autoload
(defun anki-editor-push-tree ()
  "Push all notes under a tree."
  (interactive)
  (anki-editor-push-notes 'tree))
