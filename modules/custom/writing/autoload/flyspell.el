;;; custom/writing/autoload/flyspell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun comma-smart-insert ()
  (interactive)
  (if (looking-at-p "\s")
      (progn
       (insert ",")
       (forward-char))
    (insert ", ")))
