;;; custom/general/autoload/windows.el -*- lexical-binding: t; -*-

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
(transient-define-prefix window-transient ()
  "Window manipulations"
  ["Window"
   [""
    ("k" "enlarge vertically" enlarge-window :transient t)
    ("j" "shrink vertically" shrink-window :transient t)
    ("l" "enlarge horizontally" enlarge-window-horizontally :transient t)
    ("h" "shrink horizontally" shrink-window-horizontally :transient t)]
   [""
    ("=" "balance" balance-windows)
    ("g" "golden-ratio" golden-ratio)]
   [""
    ("u" "undo" window-undo :transient t)
    ("r" "redo" window-redo :transient t)]])

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
    (funcall (doom-lookup-key (kbd "C-e")) count)))

;;;###autoload
(defun +scroll-line-up-other-window (&optional count)
  "Scrolls in the window COUNT lines downwards."
  (interactive "P")
  (with-selected-window (other-window-for-scrolling)
    (funcall (doom-lookup-key (kbd "C-y")) count)))

(defun window--layout-state ()
  "Return a canonical snapshot of window layout.
Sorted by position so comparison is independent of `window-list'
ordering (which varies with `selected-window')."
  (sort (mapcar (lambda (w)
                  (list (window-buffer w) (window-edges w)))
                (window-list))
        (lambda (a b)
          (let ((ea (cadr a)) (eb (cadr b)))
            (or (< (car ea) (car eb))
                (and (= (car ea) (car eb))
                     (< (cadr ea) (cadr eb))))))))

(defvar window--last-recorded-layout nil
  "Layout at the time of last tab-bar history recording.
Used to suppress cursor-only entries from polluting the ring.")

(define-advice tab-bar--history-change (:around (fn) skip-cursor-only)
  "Only record tab-bar history when window layout actually changes."
  (let ((layout (window--layout-state)))
    (if (equal layout window--last-recorded-layout)
        (let ((tab-bar-history-omit t))
          (funcall fn))
      (setq window--last-recorded-layout layout)
      (funcall fn))))

;;;###autoload
(defun window-undo ()
  "Undo to the previous different window layout.
Directly scans the tab-bar history back ring, skipping entries
that only differ in cursor position or selected window.  Manages
the forward ring correctly (single push).  Falls back to
`winner-undo' when tab-bar history is exhausted or unavailable."
  (interactive)
  (cond
   ((and (bound-and-true-p tab-bar-history-mode)
         (bound-and-true-p tab-bar-mode))
    (let* ((saved-wc (current-window-configuration))
           (saved-point (point-marker))
           (initial-layout (window--layout-state))
           (frame (selected-frame))
           (entries (gethash frame tab-bar-history-back))
           (pos 0)
           found-pos)
      (setq tab-bar-history-omit t)
      (catch 'done
        (dolist (entry entries)
          (let ((wc (alist-get 'wc entry)))
            (when (window-configuration-p wc)
              (set-window-configuration wc nil t)
              (unless (equal initial-layout (window--layout-state))
                (setq found-pos pos)
                (throw 'done nil))))
          (setq pos (1+ pos))))
      (if found-pos
          (let ((target (nth found-pos entries)))
            (puthash frame
                     (cons tab-bar-history-old
                           (gethash frame tab-bar-history-forward))
                     tab-bar-history-forward)
            (puthash frame
                     (nthcdr (1+ found-pos) entries)
                     tab-bar-history-back)
            (let ((wc-point (alist-get 'wc-point target)))
              (when (and (markerp wc-point) (marker-buffer wc-point))
                (goto-char wc-point)))
            (message "Window undo: %d skipped (%d back / %d fwd)"
                     found-pos
                     (length (gethash frame tab-bar-history-back))
                     (length (gethash frame tab-bar-history-forward))))
        (set-window-configuration saved-wc nil t)
        (when (and (markerp saved-point) (marker-buffer saved-point))
          (goto-char saved-point))
        (if (bound-and-true-p winner-mode)
            (winner-undo)
          (message "No window layout changes in history")))))
   ((bound-and-true-p winner-mode)
    (winner-undo))
   (t (message "No window undo mechanism available"))))

;;;###autoload
(defun window-redo ()
  "Redo to the next different window layout.
Directly scans the tab-bar history forward ring, skipping entries
that only differ in cursor position or selected window.  Manages
the back ring correctly (single push).  Falls back to
`winner-redo' when tab-bar history is exhausted or unavailable."
  (interactive)
  (cond
   ((and (bound-and-true-p tab-bar-history-mode)
         (bound-and-true-p tab-bar-mode))
    (let* ((saved-wc (current-window-configuration))
           (saved-point (point-marker))
           (initial-layout (window--layout-state))
           (frame (selected-frame))
           (entries (gethash frame tab-bar-history-forward))
           (pos 0)
           found-pos)
      (setq tab-bar-history-omit t)
      (catch 'done
        (dolist (entry entries)
          (let ((wc (alist-get 'wc entry)))
            (when (window-configuration-p wc)
              (set-window-configuration wc nil t)
              (unless (equal initial-layout (window--layout-state))
                (setq found-pos pos)
                (throw 'done nil))))
          (setq pos (1+ pos))))
      (if found-pos
          (let ((target (nth found-pos entries)))
            (puthash frame
                     (cons tab-bar-history-old
                           (gethash frame tab-bar-history-back))
                     tab-bar-history-back)
            (puthash frame
                     (nthcdr (1+ found-pos) entries)
                     tab-bar-history-forward)
            (let ((wc-point (alist-get 'wc-point target)))
              (when (and (markerp wc-point) (marker-buffer wc-point))
                (goto-char wc-point)))
            (message "Window redo: %d skipped (%d back / %d fwd)"
                     found-pos
                     (length (gethash frame tab-bar-history-back))
                     (length (gethash frame tab-bar-history-forward))))
        (set-window-configuration saved-wc nil t)
        (when (and (markerp saved-point) (marker-buffer saved-point))
          (goto-char saved-point))
        (if (bound-and-true-p winner-mode)
            (winner-redo)
          (message "No window layout changes in forward history")))))
   ((bound-and-true-p winner-mode)
    (winner-redo))
   (t (message "No window redo mechanism available"))))
