;;; e2e-driver.el --- live-Emacs harness for window layout history -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by run-e2e.sh into a real interactive `emacs -nw -Q'.  Keys
;; are injected into the genuine command loop via
;; `unread-command-events' (the same event path as typing), so
;; pre/post-command hooks, redisplay and transient dispatch all behave
;; exactly as in a live session.  State is queried externally through
;; emacsclient on a fixed socket.

;;; Code:

(setq debug-on-error nil
      inhibit-startup-screen t)

;; window-transient lists it as a suffix; irrelevant for these tests
(defun golden-ratio () (interactive))

(load (expand-file-name "../autoload/windows.el"
                        (file-name-directory load-file-name)))

(require 'server)
(setq server-name "windows-e2e"
      server-socket-dir "/tmp/windows-e2e-sock")
(server-start)

(tab-bar-mode 1)
(tab-bar-history-mode 1)
(setq tab-bar-history-limit 100)

(dolist (n '("e2e-a" "e2e-b" "e2e-c"))
  (with-current-buffer (get-buffer-create n)
    (dotimes (i 200) (insert (format "%s line %d\n" n i)))
    (goto-char (point-min))))

(switch-to-buffer "e2e-a")
(delete-other-windows)
(global-set-key (kbd "C-c w") #'window-transient)
;; popup-free bindings, like the SPC w u / SPC w r leader keys
(global-set-key (kbd "C-c u") #'window-undo)
(global-set-key (kbd "C-c r") #'window-redo)

(defvar e2e-marks (make-hash-table :test 'equal))

(defun e2e-mark (name)
  "Remember the current layout fingerprint under NAME."
  (puthash name (window-layout--norm) e2e-marks)
  t)

(defun e2e-at-mark-p (name)
  "Non-nil when the current layout is the one remembered under NAME."
  (equal (window-layout--norm) (gethash name e2e-marks)))

(defun e2e-buffer-shape (norm)
  (mapcar (lambda (x) (buffer-name (car x))) norm))

(defun e2e-at-mark-buffers-p (name)
  "Like `e2e-at-mark-p' but compares buffer arrangement only.
While the transient popup is open it steals screen space from its
siblings, so exact edges differ from marks captured without it;
buffer order (positional) still identifies each layout."
  (equal (e2e-buffer-shape (window-layout--norm))
         (e2e-buffer-shape (gethash name e2e-marks))))

(defun e2e-feed (keys)
  "Inject KEYS (in `kbd' syntax) into the real command loop."
  (setq unread-command-events
        (append unread-command-events (listify-key-sequence (kbd keys))))
  t)

(defun e2e-back-len ()
  (length (gethash (selected-frame) tab-bar-history-back)))

(defun e2e-fwd-len ()
  (length (gethash (selected-frame) tab-bar-history-forward)))

(defun e2e-transient-active-p ()
  (and (bound-and-true-p transient--prefix) t))

(defun e2e-state ()
  "Full state dump for debugging failed checks."
  (list :norm (mapcar (lambda (x) (cons (buffer-name (car x)) (cdr x)))
                      (window-layout--norm))
        :back (e2e-back-len)
        :fwd (e2e-fwd-len)
        :transient (e2e-transient-active-p)
        :point (window-point (selected-window))
        :buffer (buffer-name (window-buffer (selected-window)))
        :tab (alist-get 'name (tab-bar--current-tab))))

;;; e2e-driver.el ends here
