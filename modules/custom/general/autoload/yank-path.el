;;; custom/general/autoload/yank-path.el -*- lexical-binding: t; -*-

(require 'ffap)

(defconst +yank-path--line-suffix-regex
  "\\([^[:space:]\"'<>|*?\n]+?\\):\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?"
  "Regex matching PATH:LINE or PATH:LINE-LINE on a single line.")

;;;###autoload
(defun +yank-path--position-suffix ()
  "Return position suffix for current point/region state.
Region -> :start-end, otherwise -> :line."
  (if (use-region-p)
      (format ":%d-%d"
              (line-number-at-pos (region-beginning))
              (line-number-at-pos (region-end)))
    (format ":%d" (line-number-at-pos))))

(defun +yank-path--resolve-path (path)
  "Expand PATH and return it if it exists, else nil."
  (let ((expanded (expand-file-name (substitute-in-file-name path))))
    (and (file-exists-p expanded) expanded)))

(defun +yank-path--path-at-point ()
  "Return (list PATH START-LINE END-LINE) for a file path at point, or nil.
Scans the current line for a `PATH:LINE[-LINE]' pattern whose span
contains point.  Falls back to `ffap-file-at-point' with no line suffix
only when no `PATH:LINE' pattern covered point (so a broken suffix path
does not silently open some shorter prefix)."
  (let ((pt (point))
        (eol (line-end-position))
        result
        saw-suffix-match)
    (save-excursion
      (beginning-of-line)
      (while (and (not result)
                  (re-search-forward +yank-path--line-suffix-regex eol t))
        (when (and (<= (match-beginning 0) pt)
                   (>= (match-end 0) pt))
          (setq saw-suffix-match t)
          (when-let ((resolved (+yank-path--resolve-path
                                (match-string-no-properties 1))))
            (setq result
                  (list resolved
                        (string-to-number (match-string-no-properties 2))
                        (and (match-string-no-properties 3)
                             (string-to-number
                              (match-string-no-properties 3)))))))))
    (or result
        (unless saw-suffix-match
          (when-let* ((raw (ffap-file-at-point))
                      (resolved (+yank-path--resolve-path raw)))
            (list resolved nil nil))))))

(defun +yank-path--open-other-window (spec)
  "Open SPEC `(PATH START-LINE END-LINE)' in another window.
Jumps to START-LINE; when END-LINE is non-nil, activates the region."
  (pcase-let ((`(,path ,start ,end) spec))
    (find-file-other-window path)
    (when start
      (goto-char (point-min))
      (forward-line (1- start))
      (when end
        (push-mark (save-excursion
                     (forward-line (- end start))
                     (line-end-position))
                   nil t)))
    (message "Opened: %s%s" path
             (cond (end (format ":%d-%d" start end))
                   (start (format ":%d" start))
                   (t "")))))

;;;###autoload
(defun +yank-path--do-yank (root)
  "Yank current buffer's path (with position suffix) relative to ROOT.
In Dired, yank the path of the item at point (or directory if none)."
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

;;;###autoload
(defadvice! +yank-path--buffer-path-a (&optional root)
  "Context-aware buffer path yank / open.
With `\\[universal-argument]' prefix: always yank current buffer path.
In Dired: yank path of the item at point.
If point is on an existing file path (optionally `PATH:LINE[-LINE]'):
  open it in another window, jumping to the line (or activating range).
Otherwise: yank current buffer path with a position suffix
  :start-end  - region is active
  :line       - otherwise"
  :override #'+default/yank-buffer-path
  (cond
   ((equal current-prefix-arg '(4))
    (+yank-path--do-yank root))
   ((derived-mode-p 'dired-mode)
    (+yank-path--do-yank root))
   ((when-let ((spec (+yank-path--path-at-point)))
      (+yank-path--open-other-window spec)
      t))
   (t
    (+yank-path--do-yank root))))
