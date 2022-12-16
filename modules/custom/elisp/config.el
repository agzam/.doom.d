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
                 "m" #'erase-messages-buffer)
        (:prefix ("s" . "repl")
         :desc "messages" "s" #'+switch-to-messages-buffer-other-window
         :desc "clear " "l" #'erase-messages-buffer
         :desc "hide" "k" #'+hide-messages-window))
  (map! :map emacs-lisp-mode-map
        :g "C-c C-f" nil ; remove elisp-byte-compile-file binding
        :i "#" #'sharp-quote)

  (map! :localleader
        :map messages-buffer-mode-map
        (:prefix ("k" . "kill")
                 "m" #'erase-messages-buffer)
        (:prefix ("s" . "repl")
         :desc "clear" "l" #'erase-messages-buffer
         :desc "back to elisp" "s" #'+switch-to-last-elisp-buffer
         :desc "hide" "k" #'+hide-messages-window))

  (add-hook! 'emacs-lisp-mode-hook
    (defun always-lexical-binding-h ()
      (setq lexical-binding t))))

(after! debug
  (map! :map debugger-mode-map
        :n "e" #'debugger-eval-expression))


(after! info
  (map! :map Info-mode-map
        :n "C-j" #'Info-goto-node
        :n "^" #'Info-up
        :n "H" #'Info-history-back
        :n "L" #'Info-history-forward
        :n "C-<return>" #'Info-follow-nearest-node-new-window
        :n "n" #'Info-search-next
        :n "N" #'Info-search-backward
        :localleader
        "w" #'Info-goto-node-web
        "g" #'Info-goto-node
        "s" #'Info-search
        "i" #'Info-index
        "h" #'Info-history
        "d" #'Info-directory))


(add-to-list
 'display-buffer-alist
 `("\\*Backtrace\\*"
   (display-buffer-reuse-window
    display-buffer-in-direction)
   (direction . right)))

(after! woman
  (setq woman-manpath '("/Applications/kitty.app/Contents/Resources/man/"
                        "/usr/share/man"
                        "/usr/local/share/man"
                        "/usr/X11/man"
                        "/Library/Apple/usr/share/man")))
