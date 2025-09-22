;;; custom/elisp/config.el -*- lexical-binding: t; -*-

(map! :map profiler-report-mode-map
      :n "RET" #'profiler-report-helpful-symbol-at-point
      "M-l" #'profiler-report-expand-entry
      "M-h" #'profiler-report-collapse-entry
      "M-j" #'profiler-report-next-entry
      "M-k" #'profiler-report-previous-entry)

(add-hook! 'profiler-report-mode-hook
  (defun yas-minor-mode-off ()
    (yas-minor-mode -1))
  (run-with-timer
   0.1 nil
   #'profiler-report-expand-all))

(use-package! let-plist
  :after (elisp-mode))

(use-package! evilify-edebug
  :after (edebug evil-collection)
  :config
  (evilify-edebug-setup)
  (map! :map edebug-mode-map
        :localleader
        "ec" #'edebug-eval-current-form-sp))

(after! elisp-mode
  (add-hook! 'doom-scratch-buffer-created-hook
    (defun flycheck-off ()
      (flycheck-mode -1)))
  (map! :localleader
        :map (emacs-lisp-mode-map
              lisp-data-mode-map
              lisp-interaction-mode-map)
        :desc "Expand macro" "m" #'macrostep-expand
        (:prefix ("d" . "debug")
                 "f" #'+emacs-lisp/edebug-instrument-defun-on
                 "F" #'+emacs-lisp/edebug-instrument-defun-off)
        (:prefix ("e" . "eval")
                 "c" #'sp-eval-current-sexp
                 "b" #'eval-buffer
                 "d" #'eval-defun
                 "l" #'eval-last-sexp
                 "i" #'with-editor-eval
                 "r" #'eval-region
                 "L" #'load-library
                 "p" #'pp-eval-current
                 ";" #'eval-print-last-sexp)
        (:prefix ("g" . "goto")
                 "f" #'find-function
                 "v" #'find-variable
                 "l" #'elisp-fully-qualified-symbol-with-gh-link
                 "d" #'xref-find-definitions
                 "r" #'xref-find-references
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

  (map! :map lisp-interaction-mode-map
        :i "C-j" #'eval-print-last-sexp)

  (add-hook! 'emacs-lisp-mode-hook
             #'visual-wrap-prefix-mode
             (defun always-lexical-binding-h ()
               (setq lexical-binding t))))

(after! debug
  (map! :map debugger-mode-map
        :n "e" #'debugger-eval-expression
        :n "n" #'backtrace-forward-frame
        :n "p" #'backtrace-backward-frame
        :n "v" #'backtrace-toggle-locals))

(after! edebug
  (setq edebug-print-level nil
        edebug-print-length nil))

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
        "y" #'info-copy-node-url
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
    display-buffer-reuse-mode-window
    display-buffer-in-quadrant)
   (direction . right)
   (window . root)))

(after! woman
  (setq woman-manpath '("/Applications/kitty.app/Contents/Resources/man/"
                        "/usr/share/man"
                        "/opt/homebrew/share/man"
                        "/usr/X11/man"
                        "/Library/Apple/usr/share/man")))

(use-package! elisp-format
  :after elisp-mode)
