;;; custom/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'direx/jump-to-project-root-or-current-dir "custom/dired/autoload" nil t)
(defun direx/jump-to-project-root-or-current-dir ()
  "Open in Direx - project root if there's one, otherwise current directory."
  (interactive)
  (let ((buf (direx-project:find-project-root-noselect
              (or buffer-file-name default-directory))))
    (if buf
        (progn
          (direx:maybe-goto-current-buffer-item buf)
          (switch-to-buffer buf))
      (direx:find-directory "."))))
