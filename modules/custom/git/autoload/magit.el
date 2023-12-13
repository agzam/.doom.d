;;; custom/git/autoload/magit.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode))))
              '(display-buffer-same-window))

             ('(+magit--display-buffer-in-direction))))))

(defun +magit--display-buffer-in-direction (buffer alist)
  "`display-buffer-alist' handler that opens BUFFER in a direction.

This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
  (let ((direction (or (alist-get 'direction alist)
                       +magit-open-windows-in-direction))
        (origin-window (selected-window)))
    (if-let (window (window-in-direction direction))
        (unless magit-display-buffer-noselect
          (select-window window))
      (if-let (window (and (not (one-window-p))
                           (window-in-direction
                            (pcase direction
                              (`right 'left)
                              (`left 'right)
                              ((or `up `above) 'down)
                              ((or `down `below) 'up)))))
          (unless magit-display-buffer-noselect
            (select-window window))
        (let ((window (split-window nil nil direction)))
          (when (and (not magit-display-buffer-noselect)
                     (memq direction '(right down below)))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (unless magit-display-buffer-noselect
      (switch-to-buffer buffer t t)
      (selected-window))))


;;
;;; Auto-revert

(defvar +magit--stale-p nil)

(defun +magit--revert-buffer (buffer)
  (with-current-buffer buffer
    (kill-local-variable '+magit--stale-p)
    (when buffer-file-name
      (if (buffer-modified-p (current-buffer))
          (when (bound-and-true-p vc-mode)
            (vc-refresh-state)
            (force-mode-line-update))
        (revert-buffer t t t)))))

;;;###autoload
(defun +magit-mark-stale-buffers-h ()
  "Revert all visible buffers and mark buried buffers as stale.

Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (if (get-buffer-window buffer)
          (+magit--revert-buffer buffer)
        (with-current-buffer buffer
          (setq-local +magit--stale-p t))))))

;;;###autoload
(defun +magit-revert-buffer-maybe-h ()
  "Update `vc' and `git-gutter' if out of date."
  (when +magit--stale-p
    (+magit--revert-buffer (current-buffer))))


;;
;;; Commands

;;;###autoload
(defun +magit/quit (&optional kill-buffer)
  "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (or (cl-find-if (lambda (win)
                      (with-selected-window win
                        (and (derived-mode-p 'magit-mode)
                             (equal magit--default-directory topdir))))
                    (window-list))
        (+magit/quit-all))))

;;;###autoload
(defun +magit/quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
  (+magit-mark-stale-buffers-h))

;;;###autoload
(defun +magit--kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))


;;;###autoload
(defun +magit-log-orig_head--head (args files)
  "Compare log since the last pull. i.e.: show only commits between last pull and head"
  (interactive (magit-log-arguments))
  (magit-log-other
   (list "ORIG_HEAD..HEAD")
   (car (magit-log-arguments)) files))

;;;###autoload
(defun +magit-log-other--current (revision)
  "Compare log between branches à la GitHub style.
i.e.: show only commits that differ between selected (other branch) and current branch"
  (interactive (list (magit-read-other-branch-or-commit "Log compare")))
  (magit-log-other
   (list (concat revision ".." (magit-get-current-branch)))
   (car (magit-log-arguments)) nil))

;;;###autoload
(defun +magit-log--origin-main ()
  "Compare log between branches à la GitHub style between current branch and origin/master"
  (interactive)
  (magit-log-other
   (list (format
          "origin/%s..%s"
          (magit-main-branch)
          (magit-get-current-branch)))
   (car (magit-log-arguments)) nil))

;;;###autoload
(defun +magit-diff-range-reversed (rev-or-range &optional args files)
  "Diff between two branches. Unlike `diff-range` works in opposite order i.e.: `base..current`"
  (interactive (list (magit-read-other-branch-or-commit "Diff range")))
  (magit-diff-range (concat rev-or-range ".." (magit-get-current-branch)) args files))

;;;###autoload
(defun +magit-diff--origin-main ()
  "Compare log between branches à la GitHub style between current branch and origin/master"
  (interactive)
  (magit-diff-range
   (format
    "origin/%s..%s"
    (magit-main-branch)
    (magit-get-current-branch))))

(defun +magit-create-branch-friendly-string (sentence)
  "From a SENTENCE, e.g., GitHub issue title, form a string that can
be used as a git branch name."
  (--> sentence
       (string-split it " \\|\\[\\|\\]\\|\\:\\|{\\|}" :omit-nulls " ")
       (seq-map
        (-compose
         (-rpartial #'string-trim "_\\|-" "_\\|-")
         ;; remove the ticket number
         (lambda (x) (replace-regexp-in-string "#\\([0-9]+\\)" "" x))
         (lambda (x) (replace-regexp-in-string "_+\\|\\-+\\|'" "_" x))
         #'downcase
         (lambda (x) (replace-regexp-in-string "?\\|@\\|~\\|\\^\\|\\/\\|\\\\" "-" x)))
        it)
       (seq-remove (-rpartial #'string-match-p "_*\\| *\\|-*") it)
       (seq-take it 8)
       (string-join it "_")))

(defun +forge-select-issue ()
  "List issues of the current repository in a separate buffer."
  (let* ((repo (forge-get-repository t))
         (repo-id (oref repo id))
         (_ (when repo-id (forge-pull repo)))
         (issues-list (mapcar #'forge--format-topic-choice
                              (forge-ls-issues repo)))
         (get-issue-num (lambda (s)
                          (cadr (s-match "#\\([0-9]+\\)" s))))
         (sel (completing-read "Choose an issue: " issues-list)))
    (list sel (funcall get-issue-num sel))))

;;;###autoload
(defun +magit-worktree-branch-from-issue ()
  "Select a forge issue, and create a worktree and a branch."
  (interactive)
  (let* ((sel-issue (+forge-select-issue))
         (w-tree (format
                  "%s__%s"
                  (nth 1 sel-issue)
                  (+magit-create-branch-friendly-string (car sel-issue))))
         (worktree? (not (string-suffix-p ".bare" (magit-rev-parse "--git-dir"))))
         (def-dir (if worktree?
                      (thread-last
                        default-directory
                        (file-name-split)
                        (seq-remove #'string-blank-p)
                        (-butlast)
                        (s-join "/")
                        (format "/%s/"))
                    default-directory))
         (path (read-directory-name "Create new worktree at:" def-dir nil nil w-tree))
         (branch (magit-read-string-ns "With branch: " (thread-last
                                                         path
                                                         (file-name-split)
                                                         (last)))))
    (if  (magit-local-branch-p (format "refs/heads/%s" branch))
        (magit-run-git "worktree" "add" (magit--expand-worktree path) branch)
      (magit-run-git "worktree" "add" "-b"
                     branch (magit--expand-worktree path)
                     (magit-main-branch)))
    (magit-diff-visit-directory path)))


;;;###autoload
(defun +magit-rebase-origin-main ()
  "Fetch latest of main and rebase current branch on it"
  (interactive)
  (let* ((main (magit-main-branch))
         (remote (magit-get-remote main)))
   (magit-fetch-branch remote main nil)
   (magit-rebase-branch (format "%s/%s" remote main) nil)))
