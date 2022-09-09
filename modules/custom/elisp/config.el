;;; custom/elisp/config.el -*- lexical-binding: t; -*-

(use-package! paradox
  :defer t
  :hook (paradox-menu-mode . paradox-menu-mode--set-keys)
  :init
  (paradox-enable)
  (setq paradox-column-width-package 30))

(use-package! let-plist)

(use-package! evilify-edebug
  :after (edebug evil-collection)
  :config
  (evilify-edebug-setup)
  (map! :map edebug-mode-map
        :localleader
        "ec" #'edebug-eval-current-form-sp))

(after! elisp-mode
  (map! :localleader
        :map (emacs-lisp-mode-map lisp-data-mode-map)
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
         "c" #'eval-current-form-sp
         "p" #'pp-eval-current)
        (:prefix ("g" . "goto")
         "f" #'find-function
         "v" #'find-variable
         "l" #'find-library
         "d" #'xref-find-definitions
         "D" #'xref-find-definitions-other-window)
        (:prefix  ("h" . "help")
         "h" #'helpful-at-point)
        (:prefix ("k" . "kill")
         "m" #'erase-messages-buffer))
  (map! :map emacs-lisp-mode-map
        :g "C-c C-f" nil ; remove elisp-byte-compile-file binding
        :i "#" #'sharp-quote)

  (map! :localleader
        :map messages-buffer-mode-map
        (:prefix ("k" . "kill")
         "m" #'erase-messages-buffer)))

(after! debug
  (map! :map debugger-mode-map
        :n "e" #'debugger-eval-expression))


(after! info
  (map! :map Info-mode-map
       :n "C-j" #'Info-goto-node
       :n "^" #'Info-up))
