;;; custom/general/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun on-spacehammer-edit-with-emacs (buffer-name pid title)
  (select-frame-by-name "edit")
  (with-current-buffer (get-buffer buffer-name)
    (set-frame-parameter nil 'fullscreen nil)
    ;; need to set a filename, otherwise lsp in that buffer won't work
    (set-visited-file-name (format "/tmp/%s_%s_%s" buffer-name pid title))
    (set-buffer-modified-p nil)
    (markdown-mode)
    (evil-insert +1)))


;;;###autoload
(defun spacehammer-before-finish-edit-with-emacs (bufname pid)
  (with-current-buffer bufname
    (set-buffer-modified-p nil)))
