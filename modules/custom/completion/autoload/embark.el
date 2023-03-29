;;; custom/completion/autoload/embark.el -*- lexical-binding: t; -*-

;;;###autoload
(eval-when-compile
  (defmacro embark-ace-action (fn)
    `(defun ,(intern (concat "embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

;;;###autoload
(defmacro embark-split-action (fn split-type)
  `(defun ,(intern (concat "embark-"
                           (symbol-name fn)
                           "-"
                           (symbol-name split-type))) ()
     (interactive)
     (funcall #',split-type)
     (call-interactively #',fn)))


;;;###autoload
(defun avy-action-embark (pt)
  ;; borrowed from
  ;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun +edebug-instrument-symbol (symbol)
  (interactive "sSymbol: ")
  (edebug-instrument-function (intern symbol)))

;;;###autoload
(defun +embark-collect-outline-cycle (&optional arg)
  (interactive "P")
  (if arg (outline-cycle-buffer)
    (outline-cycle))
  (evil-beginning-of-line))
