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

;;;###autoload
(defun +lsp-completion-at-point ()
  (when lsp-mode
    (funcall 'lsp-completion-at-point)))

;; https://github.com/blahgeek/emacs-lsp-booster
;;;###autoload
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

;;;###autoload
(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
