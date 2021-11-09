;;; custom/elisp/config.el -*- lexical-binding: t; -*-

(use-package! paradox
  :hook (paradox-menu-mode . paradox-menu-mode--set-keys)
  :init
  (paradox-enable)
  (setq paradox-column-width-package 30)
  (map! :leader "hdpl" #'list-packages))

(use-package! let-plist)

(use-package! evilify-edebug
  :commands (edebug)
  :config
  (evilify-edebug-setup))

(add-hook! 'emacs-lisp-mode-hook
  (defun set-elisp-mode-keys ()
    (map! :localleader
          :map emacs-lisp-mode-map
          :desc "Expand macro" "m" #'macrostep-expand
          (:prefix ("d" . "debug")
           "f" #'+emacs-lisp/edebug-instrument-defun-on
           "F" #'+emacs-lisp/edebug-instrument-defun-off)
          (:prefix ("e" . "eval")
           "b" #'eval-buffer
           "d" #'eval-defun
           "e" #'eval-last-sexp
           "r" #'eval-region
           "l" #'load-library
           "c" #'eval-current-form-sp)
          (:prefix ("g" . "goto")
           "f" #'find-function
           "v" #'find-variable
           "l" #'find-library
           "d" #'xref-find-definitions
           "D" #'xref-find-definitions-other-window))))
