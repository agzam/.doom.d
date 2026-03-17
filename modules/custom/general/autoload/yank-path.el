;;; custom/general/autoload/yank-path.el -*- lexical-binding: t; -*-

(defun +yank-path--defun-name-at-line ()
  "Return the defun name if point is on a defun header line, nil otherwise.
Tries tree-sitter first (Emacs 29+), then falls back to `beginning-of-defun'."
  (let ((line (line-number-at-pos)))
    (save-excursion
      (or
       ;; Tree-sitter: check if defun node starts on current line
       (when (and (fboundp 'treesit-defun-at-point)
                  (ignore-errors (treesit-parser-list)))
         (when-let ((node (ignore-errors (treesit-defun-at-point))))
           (when (= (line-number-at-pos (treesit-node-start node)) line)
             (add-log-current-defun))))
       ;; Fallback: beginning-of-defun from end of line
       ;; (end-of-line ensures we find the defun starting on this line,
       ;; not the previous one)
       (progn
         (end-of-line)
         (ignore-errors
           (beginning-of-defun)
           (when (= (line-number-at-pos) line)
             (add-log-current-defun))))))))

(defun +yank-path--position-suffix ()
  "Return position suffix for current point/region state.
Region -> :start-end, defun header -> :name, otherwise -> :line."
  (cond
   ((use-region-p)
    (format ":%d-%d"
            (line-number-at-pos (region-beginning))
            (line-number-at-pos (region-end))))
   ((when-let ((name (+yank-path--defun-name-at-line)))
      (format ":%s" name)))
   (t
    (format ":%d" (line-number-at-pos)))))

;;;###autoload
(defadvice! +yank-path--buffer-path-a (&optional root)
  "Enhanced buffer path yanking with position info.
In Dired, yank the path of the item at point (or directory if none).
Otherwise, append position suffix:
  :func-name  - point on a function definition header
  :start-end  - region is active
  :line       - otherwise"
  :override #'+default/yank-buffer-path
  (if (derived-mode-p 'dired-mode)
      (let* ((filename (or (dired-get-filename nil t) default-directory))
             (path (abbreviate-file-name
                    (if root (file-relative-name filename root) filename))))
        (kill-new path)
        (message "Copied path: %s" path))
    (if-let ((filename (or (buffer-file-name (buffer-base-buffer))
                           (bound-and-true-p list-buffers-directory))))
        (let* ((base-path (abbreviate-file-name
                           (if root (file-relative-name filename root) filename)))
               (path (concat base-path (+yank-path--position-suffix))))
          (kill-new path)
          (message "Copied path: %s" path))
      (error "Couldn't find filename in current buffer"))))
