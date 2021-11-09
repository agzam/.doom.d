;;; custom/lsp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun lsp-ui-flycheck-list+ ()
  "Overrides `lsp-ui-flycheck-list', because that stupid thing
keeps wanting to control lsp-diagnostics window"
  (interactive)
  (let ((buffer (get-buffer-create "*lsp-diagnostics*"))
        (workspace lsp--cur-workspace)
        (window (selected-window)))
    (with-current-buffer buffer
      (lsp-ui-flycheck-list--update window workspace))
    (add-hook 'lsp-diagnostics-updated-hook 'lsp-ui-flycheck-list--refresh nil t)
    (setq lsp-ui-flycheck-list--buffer buffer)
    (let ((win (display-buffer
                buffer
                '((display-buffer-reuse-window
                   display-buffer-in-direction)
                  (direction . right)
                  (window . root)
                  (window-width . 0.4)))))
      ;; (set-window-dedicated-p win t)
      (select-window win)
      (fit-window-to-buffer nil nil 10))))
