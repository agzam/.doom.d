;;; custom/general/autoload/windows.el -*- lexical-binding: t; -*-

(require 'hydra)

(defun delete-other-windows-horizontally ()
  "Delete all windows to the left and right of the current
window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun toggle-window-divider ()
  (interactive)
  (setf right-divider-width (if window-divider-mode 1 6))
  (setf left-divider-width (if window-divider-mode 1 6))
  (window-divider-mode 'toggle))


;;;###autoload
(require 'transient)

;;;###autoload
(transient-define-suffix transient-window--enlarge-v ()
  :transient t
  :key "k"
  :description "enlarge vertically"
  (interactive)
  (call-interactively #'enlarge-window))

;;;###autoload
(transient-define-suffix transient-window--shrink-v ()
  :transient t
  :key "j"
  :description "shrink vertically"
  (interactive)
  (call-interactively #'shrink-window))

;;;###autoload
(transient-define-suffix transient-window--enlarge-h ()
  :transient t
  :key "l"
  :description "enlarge horizontally"
  (interactive)
  (call-interactively #'enlarge-window-horizontally))

;;;###autoload
(transient-define-suffix transient-window--shrink-h ()
  :transient t
  :key "h"
  :description "shrink horizontally"
  (interactive)
  (call-interactively #'shrink-window-horizontally))

;;;###autoload
(transient-define-suffix transient-window--balance ()
  :key "="
  :description "balance"
  (interactive)
  (call-interactively #'balance-windows))

;;;###autoload
(transient-define-suffix transient-window--golden-ratio ()
  :key "g"
  :description "golden-ratio"
  (interactive)
  (call-interactively #'golden-ratio))

;;;###autoload
(transient-define-prefix window-transient ()
  "Window manipulations"
  ["Window"
  [(transient-window--enlarge-v)
   (transient-window--shrink-v)
   (transient-window--enlarge-h)
   (transient-window--shrink-h)
   (transient-window--balance)
   (transient-window--golden-ratio)]])

;;;###autoload
(defun window-cleanup+ ()
  "Deletes duplicate windows.
Leaves single window per buffer, removing all duplicates."
  (interactive)
  (when (->>
         (window-list)
         (seq-group-by (lambda (win) (window-buffer win)))
         (seq-filter (lambda (group) (length> (cdr group) 1)))
         (seq-do (lambda (group) (seq-do #'delete-window (cddr group)))))
    (balance-windows-area)))

;;;###autoload
(defun +scroll-line-down-other-window (&optional count)
  "Scrolls in the window COUNT lines downwards."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (funcall (doom-lookup-key (kbd "C-e")) (or count 1))))

;;;###autoload
(defun +scroll-line-up-other-window (&optional count)
  "Scrolls in the window COUNT lines downwards."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (funcall (doom-lookup-key (kbd "C-y")) (or count 1))))
