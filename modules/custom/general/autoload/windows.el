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

;;; Window layout history: predictable undo/redo -------------------------
;;
;; One system, one truth: tab-bar-history-mode's per-tab rings
;; (`tab-bar-history-back' / `tab-bar-history-forward', which
;; `tab-bar--tab' stashes into each tab and `tab-bar-select-tab'
;; restores).  Stock recording and navigation are replaced wholesale:
;; stock restores cursor positions, never clears the redo ring, and
;; records from `window-configuration-change-hook', whose redisplay
;; timing batches rapid key repeats (the transient case) so recording
;; depended on typing speed.  Here every command is bracketed
;; individually on pre/post-command-hook instead.  Ring entries stay
;; shape-compatible with stock ones (alist with a `wc'
;; window-configuration) extended with a `norm' layout fingerprint.
;; winner-mode is deliberately not involved.
;;
;; The contract:
;; - one `window-undo' is exactly one distinct layout back;
;;   `window-redo' symmetrical; n undos then n redos round-trip exactly
;; - a new layout change clears the redo ring (browser back/forward)
;; - cursor position is never restored from history: after a jump every
;;   window shows its buffer at that buffer's current point
;; - history is per tab per frame; tab switches never create steps
;; - at a ring boundary: a message, zero state change
;; - consecutive changes by the same command (holding a resize key)
;;   collapse into one step
;; - transient's own popup never pollutes history

(require 'tab-bar)

(defvar window-layout--pending nil
  "List (FRAME TAB-INDEX ENTRY) captured before the current command.
The candidate for the next history step.")

(defvar window-layout--last-cmd nil
  "Last command that changed the layout; collapses same-command sprees.")

(defun window-layout--transient-buffer-p (buffer)
  "Non-nil when BUFFER is transient's popup buffer."
  (string-prefix-p " *transient*" (buffer-name buffer)))

(defun window-layout--norm (&optional frame)
  "Canonical fingerprint of FRAME's window layout.
Sorted (BUFFER . EDGES) list, skipping the minibuffer and
transient's popup.  Edges are relative to the root window, so
frame chrome coming and going (menu bar, tab bar line) does not
invalidate history.  `equal' fingerprints mean the same buffers in
the same arrangement; cursor position, scroll and selected window
deliberately don't count."
  (pcase-let ((`(,dx ,dy ,_ ,_) (window-edges (frame-root-window frame))))
    (sort (delq nil
                (mapcar (lambda (w)
                          (unless (window-layout--transient-buffer-p
                                   (window-buffer w))
                            (pcase-let ((`(,l ,top ,r ,b) (window-edges w)))
                              (list (window-buffer w)
                                    (- l dx) (- top dy) (- r dx) (- b dy)))))
                        (window-list frame 'nomini)))
          (lambda (a b)
            (let ((ea (cdr a)) (eb (cdr b)))
              (if (= (car ea) (car eb))
                  (< (cadr ea) (cadr eb))
                (< (car ea) (car eb))))))))

(defun window-layout--transient-live-p (&optional frame)
  "Non-nil when a transient popup window is live on FRAME."
  (and (seq-some (lambda (w)
                   (window-layout--transient-buffer-p (window-buffer w)))
                 (window-list frame 'nomini))
       t))

(defun window-layout--entry (&optional frame)
  "Snapshot FRAME's layout as a history entry.
Shape-compatible with stock tab-bar history entries (`wc' key), so
`tab-bar--tab' / `tab-bar-select-tab' keep stashing and restoring
the rings per tab without knowing about us."
  `((wc . ,(current-window-configuration frame))
    (norm . ,(window-layout--norm frame))))

;;;###autoload
(defun window-layout--pre-change ()
  "Open the command bracket: snapshot the layout it might change.
Installed as `:override' advice on `tab-bar--history-pre-change',
which `tab-bar-history-mode' puts on `pre-command-hook'."
  (condition-case err
      (progn
        (setq tab-bar-history-omit nil)
        (when (zerop (minibuffer-depth))
          (setq window-layout--pending
                (list (selected-frame)
                      (tab-bar--current-tab-index)
                      (window-layout--transient-live-p)
                      (window-layout--entry)))))
    (error (message "window-layout--pre-change: %S" err))))

;;;###autoload
(defun window-layout--record ()
  "Close the command bracket, recording at most one history step.
Runs on `post-command-hook' so recording is per command and
independent of redisplay timing.  A step is recorded only when this
command changed the layout fingerprint of the same frame and tab,
wasn't marked to be omitted (jumps, tab switches), and isn't a
repeat of the command that produced the previous step.  Any new
step invalidates the redo ring; that is what makes redo
predictable.  Errors are contained because Emacs silently drops
misbehaving `post-command-hook' functions."
  (condition-case err
      (when (zerop (minibuffer-depth))
        (pcase-let ((`(,frame ,tab-index ,popup ,pending) window-layout--pending))
          (cond
           ((null pending) nil)
           ;; the command moved to another frame or tab: never record
           ;; across, and break any collapse chain
           ((or (not (eq frame (selected-frame)))
                (not (eql tab-index (tab-bar--current-tab-index))))
            (setq window-layout--last-cmd nil))
           ;; a transient popup opened or closed during this command; its
           ;; siblings got resized by the popup, not by the user
           ((not (eq popup (window-layout--transient-live-p frame)))
            (setq window-layout--last-cmd nil))
           ;; nothing layout-wise happened
           ((equal (alist-get 'norm pending) (window-layout--norm frame))
            nil)
           ;; note: no tab-bar-history-mode guard needed; with the mode
           ;; off its pre-command hook is gone, so `pending' stays nil
           (t
            (when (and (not tab-bar-history-omit)
                       (not (eq this-command window-layout--last-cmd)))
              (unless (equal (alist-get 'norm pending)
                             (alist-get 'norm
                                        (car (gethash frame tab-bar-history-back))))
                (puthash frame
                         (seq-take (cons pending
                                         (gethash frame tab-bar-history-back))
                                   tab-bar-history-limit)
                         tab-bar-history-back))
              (puthash frame nil tab-bar-history-forward))
            (setq window-layout--last-cmd this-command))))
        (setq window-layout--pending nil))
    (error (message "window-layout--record: %S" err))))

;;;###autoload
(with-eval-after-load 'tab-bar
  (advice-add 'tab-bar--history-pre-change :override #'window-layout--pre-change)
  ;; stock recorder is redisplay-driven and would double-record; fully
  ;; replaced by the post-command recorder
  (advice-add 'tab-bar--history-change :override #'ignore)
  (add-hook 'post-command-hook #'window-layout--record))

(defun window-layout--apply (entry)
  "Restore ENTRY's window configuration without touching cursor memory.
`set-window-configuration' rewinds window points, and the point of
the then-current buffer, to their values at capture time - the
single most disorienting behavior of stock tab-bar/winner undo.  So
buffer points are snapshotted and put back, then every window is
moved to its buffer's own point.  Stale transient popups embedded
in old snapshots are dropped unless a transient is active."
  (let ((points (mapcar (lambda (b) (cons b (with-current-buffer b (point))))
                        (buffer-list))))
    (set-window-configuration (alist-get 'wc entry) nil t)
    (dolist (bp points)
      (when (buffer-live-p (car bp))
        (with-current-buffer (car bp)
          (goto-char (min (cdr bp) (point-max)))))))
  (unless (bound-and-true-p transient--prefix)
    (dolist (w (window-list nil 'nomini))
      (when (and (window-layout--transient-buffer-p (window-buffer w))
                 (not (eq w (frame-root-window))))
        (delete-window w))))
  (dolist (w (window-list nil 'nomini))
    (set-window-point w (with-current-buffer (window-buffer w) (point)))))

(defun window-layout--jump (from to empty-msg)
  "Move one layout step: pop FROM, push the current layout onto TO.
Entries indistinguishable from the current layout are discarded so
a jump never visibly does nothing.  EMPTY-MSG is shown when FROM
has nothing to offer, and then nothing changes."
  (setq tab-bar-history-omit t)
  (let* ((frame (selected-frame))
         (current (window-layout--norm frame))
         (entries (gethash frame from)))
    (while (and entries (equal (alist-get 'norm (car entries)) current))
      (setq entries (cdr entries)))
    (if (null entries)
        (progn
          (puthash frame nil from)
          (message "%s" empty-msg))
      (puthash frame (cons (window-layout--entry frame) (gethash frame to)) to)
      (puthash frame (cdr entries) from)
      (window-layout--apply (car entries))
      (message "Window layout: %d back, %d forward"
               (length (gethash frame tab-bar-history-back))
               (length (gethash frame tab-bar-history-forward))))))

;;;###autoload
(defun window-undo ()
  "Go back one distinct window layout in this tab's history."
  (interactive)
  (window-layout--jump tab-bar-history-back tab-bar-history-forward
                       "No earlier window layout"))

;;;###autoload
(defun window-redo ()
  "Go forward one distinct window layout in this tab's history."
  (interactive)
  (window-layout--jump tab-bar-history-forward tab-bar-history-back
                       "No further window layout"))
