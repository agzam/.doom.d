;;; custom/modeline/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun setup-custom-doom-modeline ()
  (require 'doom-modeline)
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
      buffer-position))
    (doom-modeline-set-modeline 'agcustom))
