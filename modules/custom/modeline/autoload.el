;;; custom/modeline/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun setup-custom-doom-modeline ()
  (require 'doom-modeline)
  ;; Only apply custom modeline if doom-modeline-mode is actually enabled
  (when doom-modeline-mode
    ;; Don't override modeline for buffers that set their own mode-line-format
    (unless (and (local-variable-p 'mode-line-format)
                 (not (equal mode-line-format (default-value 'mode-line-format))))
      (doom-modeline-def-modeline
        'agcustom
        '(bar persp-name window-number buffer-info)
        '(;; battery
          ;; grip
          ;; irc mu4e debug
          ;; repl
          major-mode
          lsp
          misc-info
          process
          pdf-pages
          ;; checker

          matches selection-info
          input-method
          buffer-position))
      (doom-modeline-set-modeline 'agcustom))))
