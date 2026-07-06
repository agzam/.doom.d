;;; windows-test.el --- Tests for window layout undo/redo -*- lexical-binding: t; -*-

;;; Commentary:
;; Contract tests for `window-undo' / `window-redo' (autoload/windows.el).
;;
;; Run:  make test
;; or:   emacs -Q --batch -l ert -l windows-test.el -f ert-run-tests-batch-and-exit
;;
;; Batch Emacs has no command loop and no redisplay, so the pre/post
;; command hooks never run on their own.  `windows-test--cmd' emulates
;; one command-loop iteration exactly the way the interactive session
;; brackets the recorder: buffer sync, pre-command capture, command
;; body, post-command record.  The real hook wiring is exercised by the
;; e2e suite (e2e-driver.el + run-e2e.sh), which drives a live terminal
;; Emacs through its real command loop.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(defvar windows-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "../autoload/windows.el" windows-test--dir) nil t)

(setq tab-bar-history-limit 100)

;;; Harness -----------------------------------------------------------------

(defun windows-test--buf (name)
  "Get or create test buffer NAME with content to move around in."
  (or (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (insert (make-string 300 ?x))
        (goto-char 150)
        (current-buffer))))

(defun windows-test--reset ()
  "Pristine single-window layout and empty history state."
  ;; pin geometry: frame chrome (menu/tab bar lines) shifts in batch
  (ignore-errors (set-frame-size (selected-frame) 80 25))
  (delete-other-windows)
  (set-window-buffer (selected-window) (windows-test--buf "wt-a"))
  (set-buffer (window-buffer (selected-window)))
  (clrhash tab-bar-history-back)
  (clrhash tab-bar-history-forward)
  (setq window-layout--pending nil
        window-layout--last-cmd nil
        tab-bar-history-omit nil))

(defun windows-test--cmd (cmd thunk)
  "Emulate one command-loop iteration running THUNK as command CMD."
  (let ((this-command cmd))
    ;; the command loop syncs current buffer to the selected window
    (set-buffer (window-buffer (selected-window)))
    (window-layout--pre-change)
    (funcall thunk)
    (window-layout--record)))

(defun windows-test--ring-norms (hash)
  "The `norm' of every entry in HASH's ring for the selected frame."
  (mapcar (lambda (e) (alist-get 'norm e))
          (gethash (selected-frame) hash)))

(defmacro windows-test--collecting-messages (&rest body)
  "Execute BODY, returning the list of messages it emitted, newest first."
  `(let ((collected nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) collected))
                  nil)))
       ,@body)
     collected))

;;; Deterministic contract tests --------------------------------------------

(ert-deftest windows-test-undo-restores-previous-layout ()
  (windows-test--reset)
  (let ((l0 (window-layout--norm)))
    (windows-test--cmd 'split (lambda () (split-window-below)))
    (should-not (equal l0 (window-layout--norm)))
    (windows-test--cmd 'window-undo #'window-undo)
    (should (equal l0 (window-layout--norm)))))

(ert-deftest windows-test-exact-round-trip ()
  "N undos then n redos with nothing in between is an exact round trip."
  (windows-test--reset)
  (let (norms)
    (push (window-layout--norm) norms)
    (windows-test--cmd 'split (lambda () (split-window-below)))
    (push (window-layout--norm) norms)
    (windows-test--cmd 'split-r (lambda () (split-window-right)))
    (push (window-layout--norm) norms)
    (windows-test--cmd 'buf (lambda ()
                              (set-window-buffer (selected-window)
                                                 (windows-test--buf "wt-b"))))
    (push (window-layout--norm) norms)
    (setq norms (nreverse norms))       ; (L0 L1 L2 L3)
    (should (= 3 (length (windows-test--ring-norms tab-bar-history-back))))
    (dolist (target (list (nth 2 norms) (nth 1 norms) (nth 0 norms)))
      (windows-test--cmd 'window-undo #'window-undo)
      (should (equal target (window-layout--norm))))
    (should (= 3 (length (windows-test--ring-norms tab-bar-history-forward))))
    (dolist (target (list (nth 1 norms) (nth 2 norms) (nth 3 norms)))
      (windows-test--cmd 'window-redo #'window-redo)
      (should (equal target (window-layout--norm))))
    (should (null (windows-test--ring-norms tab-bar-history-forward)))))

(ert-deftest windows-test-undo-at-boundary-is-noop ()
  (windows-test--reset)
  (let* ((l0 (window-layout--norm))
         (msgs (windows-test--collecting-messages
                (windows-test--cmd 'window-undo #'window-undo))))
    (should (member "No earlier window layout" msgs))
    (should (equal l0 (window-layout--norm)))
    ;; exhaust a real history, then hit the boundary again
    (windows-test--cmd 'split (lambda () (split-window-below)))
    (windows-test--cmd 'window-undo #'window-undo)
    (setq msgs (windows-test--collecting-messages
                (windows-test--cmd 'window-undo #'window-undo)))
    (should (member "No earlier window layout" msgs))
    (should (equal l0 (window-layout--norm)))))

(ert-deftest windows-test-redo-at-boundary-is-noop ()
  (windows-test--reset)
  (windows-test--cmd 'split (lambda () (split-window-below)))
  (let* ((l1 (window-layout--norm))
         (msgs (windows-test--collecting-messages
                (windows-test--cmd 'window-redo #'window-redo))))
    (should (member "No further window layout" msgs))
    (should (equal l1 (window-layout--norm)))))

(ert-deftest windows-test-new-change-clears-redo ()
  (windows-test--reset)
  (windows-test--cmd 'split (lambda () (split-window-below)))
  (windows-test--cmd 'split-r (lambda () (split-window-right)))
  (windows-test--cmd 'window-undo #'window-undo)
  (should (= 1 (length (windows-test--ring-norms tab-bar-history-forward))))
  (windows-test--cmd 'buf (lambda ()
                            (set-window-buffer (selected-window)
                                               (windows-test--buf "wt-c"))))
  (should (null (windows-test--ring-norms tab-bar-history-forward)))
  (let* ((l3 (window-layout--norm))
         (msgs (windows-test--collecting-messages
                (windows-test--cmd 'window-redo #'window-redo))))
    (should (member "No further window layout" msgs))
    (should (equal l3 (window-layout--norm)))))

(ert-deftest windows-test-point-motion-never-a-step ()
  (windows-test--reset)
  (let ((l0 (window-layout--norm)))
    (windows-test--cmd 'split (lambda () (split-window-below)))
    (dotimes (i 3)
      (windows-test--cmd 'move (lambda () (goto-char (+ 1 (* 40 (1+ i)))))))
    (should (= 1 (length (windows-test--ring-norms tab-bar-history-back))))
    (windows-test--cmd 'window-undo #'window-undo)
    (should (equal l0 (window-layout--norm)))
    (windows-test--cmd 'window-undo #'window-undo)
    (should (equal l0 (window-layout--norm)))))

(ert-deftest windows-test-same-command-spree-is-one-step ()
  "Holding a resize key produces one history step, not one per repeat."
  (windows-test--reset)
  (windows-test--cmd 'split (lambda () (split-window-below)))
  (let ((pre-spree (window-layout--norm)))
    (dotimes (_ 4)
      (windows-test--cmd 'enlarge-window (lambda () (enlarge-window 1))))
    (should-not (equal pre-spree (window-layout--norm)))
    (should (= 2 (length (windows-test--ring-norms tab-bar-history-back))))
    (windows-test--cmd 'window-undo #'window-undo)
    (should (equal pre-spree (window-layout--norm)))))

(ert-deftest windows-test-spree-after-undo-is-a-new-step ()
  "An undo breaks the same-command collapse chain."
  (windows-test--reset)
  (windows-test--cmd 'split (lambda () (split-window-below)))
  (let ((l1 (window-layout--norm)))
    (dotimes (_ 3)
      (windows-test--cmd 'enlarge-window (lambda () (enlarge-window 1))))
    (windows-test--cmd 'window-undo #'window-undo)
    (should (equal l1 (window-layout--norm)))
    ;; resuming the same command after an undo must record a new step
    (dotimes (_ 2)
      (windows-test--cmd 'enlarge-window (lambda () (enlarge-window 1))))
    (windows-test--cmd 'window-undo #'window-undo)
    (should (equal l1 (window-layout--norm)))))

(ert-deftest windows-test-cursor-position-not-restored ()
  "Undo restores the layout but leaves cursors where the user left them."
  (windows-test--reset)
  (windows-test--cmd 'move (lambda () (goto-char 100)))
  (windows-test--cmd 'split (lambda () (split-window-below)))
  (windows-test--cmd 'move2 (lambda () (goto-char 7)))
  (windows-test--cmd 'window-undo #'window-undo)
  ;; stock behavior would rewind the cursor to 100
  (should (= 7 (window-point (selected-window))))
  (should (= 7 (with-current-buffer "wt-a" (point)))))

(ert-deftest windows-test-killed-buffer-does-not-break-undo ()
  (windows-test--reset)
  (let ((doomed (get-buffer-create "wt-doomed")))
    (with-current-buffer doomed (insert (make-string 100 ?d)))
    (windows-test--cmd 'split (lambda () (split-window-below)))
    (windows-test--cmd 'buf (lambda () (set-window-buffer (selected-window) doomed)))
    (windows-test--cmd 'kill (lambda () (kill-buffer doomed)))
    ;; jump into a layout that references the dead buffer: no error, and
    ;; the window arrangement (2 windows) is still restored
    (windows-test--cmd 'window-undo #'window-undo)
    (should (= 2 (length (window-layout--norm))))
    (windows-test--cmd 'window-undo #'window-undo)
    (should (= 1 (length (window-layout--norm))))))

(ert-deftest windows-test-history-limit-respected ()
  (windows-test--reset)
  (let ((tab-bar-history-limit 5))
    (dotimes (i 12)
      (windows-test--cmd (intern (format "op-%d" i))
                         (lambda ()
                           (if (one-window-p)
                               (split-window-below)
                             (delete-window)))))
    (should (= 5 (length (windows-test--ring-norms tab-bar-history-back))))))

(ert-deftest windows-test-ring-entries-stay-stock-compatible ()
  "tab-bar-select-tab only stashes/restores rings whose entries carry a
window-configuration under `wc'; our entries must keep that shape."
  (windows-test--reset)
  (windows-test--cmd 'split (lambda () (split-window-below)))
  (let ((entry (car (gethash (selected-frame) tab-bar-history-back))))
    (should (window-configuration-p (alist-get 'wc entry)))
    (should (alist-get 'norm entry))))

(ert-deftest windows-test-transient-popup-never-a-step ()
  "Opening/closing the transient popup resizes its siblings; still no steps."
  (windows-test--reset)
  (let ((l0 (window-layout--norm))
        (tbuf (get-buffer-create " *transient*")))
    (windows-test--cmd 'transient-show
                       (lambda () (set-window-buffer (split-window-below) tbuf)))
    (should (null (windows-test--ring-norms tab-bar-history-back)))
    ;; resizing while the popup is open is a real user action: one step
    (windows-test--cmd 'enlarge-window (lambda () (enlarge-window 2)))
    (should (= 1 (length (windows-test--ring-norms tab-bar-history-back))))
    (windows-test--cmd 'transient-hide
                       (lambda () (delete-window (get-buffer-window tbuf))))
    (should (= 1 (length (windows-test--ring-norms tab-bar-history-back))))
    ;; the popup transitions themselves never became steps
    (should-not (member l0 (windows-test--ring-norms tab-bar-history-forward)))))

(ert-deftest windows-test-stale-transient-window-dropped-on-apply ()
  (windows-test--reset)
  (let ((tbuf (get-buffer-create " *transient*"))
        entry)
    (set-window-buffer (split-window-below) tbuf)
    (setq entry (window-layout--entry))    ; snapshot embeds the popup
    (delete-other-windows)
    (window-layout--apply entry)
    (should-not (seq-some (lambda (w) (eq (window-buffer w) tbuf))
                          (window-list nil 'nomini)))
    (should (= 1 (length (window-list nil 'nomini))))))

(ert-deftest windows-test-per-tab-isolation ()
  "Each tab has its own history; switching tabs never creates steps."
  (windows-test--reset)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  ;; enabling the modes resizes the batch frame; re-pin geometry
  (windows-test--reset)
  (unwind-protect
      (progn
        (windows-test--cmd 'split (lambda () (split-window-below)))
        (let ((tab1-l1 (window-layout--norm))
              (tab1-back (windows-test--ring-norms tab-bar-history-back)))
          (should (= 1 (length tab1-back)))
          ;; a fresh tab starts with empty rings
          (windows-test--cmd 'new-tab (lambda () (tab-bar-new-tab)))
          (should (null (windows-test--ring-norms tab-bar-history-back)))
          (should (null (windows-test--ring-norms tab-bar-history-forward)))
          ;; steps made in tab 2 stay in tab 2
          (windows-test--cmd 'split-r (lambda () (split-window-right)))
          (windows-test--cmd 'buf (lambda ()
                                    (set-window-buffer (selected-window)
                                                       (windows-test--buf "wt-b"))))
          (should (= 2 (length (windows-test--ring-norms tab-bar-history-back))))
          (windows-test--cmd 'window-undo #'window-undo)
          (should (= 1 (length (windows-test--ring-norms tab-bar-history-back))))
          ;; back in tab 1: its ring is intact and undo works on its own past
          (windows-test--cmd 'switch-tab (lambda () (tab-bar-select-tab 1)))
          (should (equal tab1-back (windows-test--ring-norms tab-bar-history-back)))
          (should (equal tab1-l1 (window-layout--norm)))
          (windows-test--cmd 'window-undo #'window-undo)
          (should (null (windows-test--ring-norms tab-bar-history-back)))))
    (tab-bar-close-other-tabs)
    (tab-bar-history-mode -1)
    (tab-bar-mode -1)))

;;; Model-based randomized tests ---------------------------------------------
;;
;; The model is an independent restatement of the contract: back ring,
;; current layout, forward ring, plus the collapse marker.  Random walks
;; interleave layout mutations with undos/redos; after every single step
;; the live fingerprint and BOTH rings must equal the model exactly.

(cl-defstruct windows-test-model back cur fwd done)

(defun windows-test--model-op (m cmd new-norm)
  "Advance model M for command CMD that left the layout at NEW-NORM.
Contract: a step is recorded per layout-changing command unless it
repeats the previous step's command (spree collapse); the ring
never gains an entry equal to its head (steps are distinct); any
real change clears the redo ring."
  (unless (equal new-norm (windows-test-model-cur m))
    (unless (eq cmd (windows-test-model-done m))
      (unless (equal (windows-test-model-cur m)
                     (car (windows-test-model-back m)))
        (setf (windows-test-model-back m)
              (seq-take (cons (windows-test-model-cur m)
                              (windows-test-model-back m))
                        tab-bar-history-limit)))
      (setf (windows-test-model-fwd m) nil))
    (setf (windows-test-model-done m) cmd)
    (setf (windows-test-model-cur m) new-norm)))

(defun windows-test--model-jump (m from-getter from-setter to-getter to-setter cmd)
  "Contract: a jump drops entries indistinguishable from the current
layout, then moves one distinct step; at the boundary nothing changes."
  (let ((from (funcall from-getter m))
        (cur (windows-test-model-cur m)))
    (while (and from (equal (car from) cur))
      (setq from (cdr from)))
    (if (null from)
        (funcall from-setter m nil)
      (funcall to-setter m (cons cur (funcall to-getter m)))
      (setf (windows-test-model-cur m) (car from))
      (funcall from-setter m (cdr from))
      (setf (windows-test-model-done m) cmd))))

(defun windows-test--model-undo (m)
  (windows-test--model-jump m
                            #'windows-test-model-back
                            (lambda (m v) (setf (windows-test-model-back m) v))
                            #'windows-test-model-fwd
                            (lambda (m v) (setf (windows-test-model-fwd m) v))
                            'window-undo))

(defun windows-test--model-redo (m)
  (windows-test--model-jump m
                            #'windows-test-model-fwd
                            (lambda (m v) (setf (windows-test-model-fwd m) v))
                            #'windows-test-model-back
                            (lambda (m v) (setf (windows-test-model-back m) v))
                            'window-redo))

(defvar windows-test--ops
  `((split-below . ,(lambda () (ignore-errors (split-window-below))))
    (split-right . ,(lambda () (ignore-errors (split-window-right))))
    (delete-win . ,(lambda () (unless (one-window-p) (delete-window))))
    (delete-others . ,(lambda () (delete-other-windows)))
    (buf-b . ,(lambda () (set-window-buffer (selected-window)
                                            (windows-test--buf "wt-b"))))
    (buf-c . ,(lambda () (set-window-buffer (selected-window)
                                            (windows-test--buf "wt-c"))))
    (other-win . ,(lambda () (other-window 1)))
    (enlarge . ,(lambda () (ignore-errors (enlarge-window 1))))
    (shrink . ,(lambda () (ignore-errors (shrink-window 1))))
    (move-point . ,(lambda () (goto-char (max 1 (random (point-max))))))
    (balance . ,(lambda () (ignore-errors (balance-windows)))))
  "Layout mutations for random walks; each must be safe to no-op.")

(defun windows-test--assert-model (m)
  "Live layout and both rings must match model M exactly."
  (should (equal (window-layout--norm) (windows-test-model-cur m)))
  (should (equal (windows-test--ring-norms tab-bar-history-back)
                 (windows-test-model-back m)))
  (should (equal (windows-test--ring-norms tab-bar-history-forward)
                 (windows-test-model-fwd m))))

(defvar windows-test--walk-seeds 100
  "How many randomized scenarios to run.")
(defvar windows-test--walk-steps 30
  "How many steps per scenario.")

(ert-deftest windows-test-model-random-walks ()
  "Seeded random scenarios, live state checked against the model each step."
  (dotimes (seed windows-test--walk-seeds)
    (windows-test--reset)
    (random (format "windows-test-%d" seed))
    (let ((m (make-windows-test-model :back nil
                                      :cur (window-layout--norm)
                                      :fwd nil
                                      :done nil)))
      (dotimes (step windows-test--walk-steps)
        (let ((dice (random 100)))
          (cond
           ((< dice 60)
            (let ((op (nth (random (length windows-test--ops)) windows-test--ops)))
              (windows-test--cmd (car op) (cdr op))
              (windows-test--model-op m (car op) (window-layout--norm))))
           ((< dice 80)
            (windows-test--cmd 'window-undo #'window-undo)
            (windows-test--model-undo m))
           (t
            (windows-test--cmd 'window-redo #'window-redo)
            (windows-test--model-redo m))))
        (ert-info ((format "seed %d, step %d" seed step))
          (windows-test--assert-model m))))))

(provide 'windows-test)
;;; windows-test.el ends here
