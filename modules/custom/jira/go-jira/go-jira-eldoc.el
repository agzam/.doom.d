;;; go-jira-eldoc.el --- Eldoc integration for go-jira -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, jira
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Eldoc integration to show Jira ticket descriptions on hover in normal mode.
;; Also provides posframe popup support to display descriptions in GUI Emacs.

;;; Code:

(require 'go-jira)

(defvar go-jira-eldoc-cache (make-hash-table :test 'equal)
  "Cache for Jira ticket descriptions to avoid repeated API calls.")

(defvar go-jira-eldoc-cache-ttl 3600
  "Time-to-live for cache entries in seconds (default: 1 hour).")

(defvar go-jira-popup-max-width 80
  "Maximum width for popup description text.")

(defvar go-jira-popup-buffer " *jira-ticket-popup*"
  "Buffer name for Jira ticket popup.")

(defface go-jira-popup-face
  '((t :inherit default))
  "Face for Jira ticket description popup."
  :group 'go-jira)

(defface go-jira-popup-border-face
  '((t :inherit font-lock-comment-face))
  "Face for Jira popup border."
  :group 'go-jira)

(defvar-local go-jira--last-ticket-pos nil
  "Last position where we showed a Jira ticket popup.")

(defvar go-jira--posframe-available-p nil
  "Whether posframe is available.")

(setq go-jira--posframe-available-p
      (and (display-graphic-p)
           (require 'posframe nil t)))

;;; Internal cache functions

(defun go-jira-eldoc--cache-get (ticket)
  "Get TICKET description from cache if valid."
  (when-let* ((entry (gethash ticket go-jira-eldoc-cache))
              (timestamp (car entry))
              (description (cdr entry)))
    (if (< (- (float-time) timestamp) go-jira-eldoc-cache-ttl)
        description
      (remhash ticket go-jira-eldoc-cache)
      nil)))

(defun go-jira-eldoc--cache-put (ticket description)
  "Store TICKET DESCRIPTION in cache with current timestamp."
  (puthash ticket (cons (float-time) description) go-jira-eldoc-cache))

(defun go-jira-eldoc--clear-cache ()
  "Clear the entire Jira eldoc cache."
  (interactive)
  (clrhash go-jira-eldoc-cache)
  (message "Jira eldoc cache cleared"))

(defun go-jira-eldoc--fetch-description (ticket)
  "Fetch description for TICKET, using cache when available."
  (or (go-jira-eldoc--cache-get ticket)
      (when-let ((summary (condition-case nil
                              (go-jira-summary ticket)
                            (error nil))))
        (go-jira-eldoc--cache-put ticket summary)
        summary)))

(defun go-jira-eldoc--ticket-at-point ()
  "Return Jira ticket at point if present, nil otherwise."
  (when-let* ((ticket-pattern "\\b[A-Z]\\{2,10\\}-[0-9]+\\b")
              (thing (thing-at-point 'symbol t)))
    (when (string-match-p (concat "\\`" ticket-pattern "\\'") thing)
      thing)))

(defun go-jira-eldoc--ticket-bounds-at-point ()
  "Return (TICKET . (BEG . END)) for ticket at point, or nil."
  (when-let* ((ticket-pattern "\\b[A-Z]\\{2,10\\}-[0-9]+\\b")
              (bounds (bounds-of-thing-at-point 'symbol))
              (beg (car bounds))
              (end (cdr bounds))
              (ticket (buffer-substring-no-properties beg end)))
    (when (string-match-p (concat "\\`" ticket-pattern "\\'") ticket)
      (cons ticket bounds))))

;;; Posframe popup functions

(defun go-jira-popup--format-description (ticket description)
  "Format TICKET and DESCRIPTION for popup display."
  (let* ((ticket-bold (propertize ticket 'face '(:weight bold)))
         (full-text (format "%s: %s" ticket-bold description))
         (max-width go-jira-popup-max-width))
    (with-temp-buffer
      (insert full-text)
      (fill-region (point-min) (point-max) max-width)
      (buffer-string))))

(defun go-jira-popup--hide ()
  "Hide the Jira ticket popup."
  (when go-jira--posframe-available-p
    (posframe-hide go-jira-popup-buffer))
  (setq go-jira--last-ticket-pos nil))

(defun go-jira-popup--maybe-hide ()
  "Hide popup if it's visible but we're not in the source buffer anymore."
  (when (and go-jira--posframe-available-p
             (posframe-workable-p)
             (get-buffer go-jira-popup-buffer))
    (let* ((popup-buf (get-buffer go-jira-popup-buffer))
           (frame (buffer-local-value 'posframe--frame popup-buf)))
      ;; Check if frame is alive before using it
      (when (and frame (frame-live-p frame) (frame-visible-p frame))
        (let ((source-buf (buffer-local-value 'go-jira--popup-source-buffer popup-buf)))
          ;; Hide if we're not in the source buffer or source buffer isn't visible
          (when (or (not (eq (current-buffer) source-buf))
                    (not (get-buffer-window source-buf)))
            (go-jira-popup--hide)))))))

(defun go-jira-popup--show (ticket description)
  "Show DESCRIPTION for TICKET in a posframe popup."
  (if go-jira--posframe-available-p
      (let ((text (go-jira-popup--format-description ticket description))
            (current-buf (current-buffer)))
        (posframe-show
         go-jira-popup-buffer
         :string text
         :position (point)
         :poshandler #'posframe-poshandler-point-bottom-left-corner
         :border-width 1
         :border-color (face-foreground 'go-jira-popup-border-face nil t)
         :background-color (face-background 'default nil t)
         :foreground-color (face-foreground 'default nil t)
         :internal-border-width 12
         :internal-border-color (face-background 'default nil t)
         :left-fringe 8
         :right-fringe 8
         :override-parameters '((no-accept-focus . t)))
        ;; Store which buffer this popup belongs to
        (with-current-buffer go-jira-popup-buffer
          (setq-local go-jira--popup-source-buffer current-buf))
        (setq go-jira--last-ticket-pos (point)))
    ;; Fallback: do nothing, eldoc will handle it
    nil))

(defun go-jira-popup--update ()
  "Update Jira popup for ticket at point."
  (if-let* ((ticket-info (go-jira-eldoc--ticket-bounds-at-point))
            (ticket (car ticket-info))
            ;; Only show in normal state
            (_ (and (bound-and-true-p evil-mode)
                    (eq evil-state 'normal))))
      (progn
        ;; Only update if we moved to a different position
        (unless (and go-jira--last-ticket-pos
                     (= (point) go-jira--last-ticket-pos))
          ;; Try to get from cache immediately
          (if-let ((description (go-jira-eldoc--cache-get ticket)))
              (go-jira-popup--show ticket description)
            ;; Fetch async if not in cache
            (run-with-idle-timer
             0.2 nil
             (lambda ()
               (when (and (eq (current-buffer) (window-buffer))
                          (go-jira-eldoc--ticket-at-point))
                 (when-let ((desc (go-jira-eldoc--fetch-description ticket)))
                   (go-jira-popup--show ticket desc))))))))
    ;; Not on a ticket, hide popup
    (go-jira-popup--hide)))

;;; Public API

;;;###autoload
(defun go-jira-eldoc-function (callback &rest _ignored)
  "Eldoc documentation function for Jira tickets.
Calls CALLBACK with the ticket description when point is on a Jira ticket.
Designed to work with eldoc-documentation-functions."
  (when-let* ((ticket (go-jira-eldoc--ticket-at-point))
              ;; Only fetch if in normal state (not while typing)
              (_ (and (bound-and-true-p evil-mode)
                      (eq evil-state 'normal))))
    ;; Fetch asynchronously to avoid blocking
    (run-with-idle-timer
     0.1 nil
     (lambda ()
       (when-let ((description (go-jira-eldoc--fetch-description ticket)))
         (funcall callback
                  (format "%s: %s" ticket description)
                  :thing ticket
                  :face 'font-lock-doc-face))))))

;;;###autoload
(define-minor-mode go-jira-eldoc-mode
  "Minor mode to show Jira ticket descriptions in eldoc."
  :global nil
  :lighter nil
  (if go-jira-eldoc-mode
      (add-hook 'eldoc-documentation-functions #'go-jira-eldoc-function nil t)
    (remove-hook 'eldoc-documentation-functions #'go-jira-eldoc-function t)))

;;;###autoload
(defun go-jira-eldoc-enable ()
  "Enable go-jira-eldoc-mode in current buffer."
  (interactive)
  (go-jira-eldoc-mode 1)
  (eldoc-mode 1))

;;;###autoload
(define-minor-mode go-jira-popup-mode
  "Minor mode to show Jira ticket descriptions in posframe popups."
  :global nil
  :lighter nil
  (if go-jira-popup-mode
      (progn
        (add-hook 'post-command-hook #'go-jira-popup--update nil t)
        ;; Clean up when entering insert mode
        (when (bound-and-true-p evil-mode)
          (add-hook 'evil-insert-state-entry-hook #'go-jira-popup--hide nil t))
        ;; Hide popup when switching buffers/tabs/windows (global hook)
        (add-hook 'buffer-list-update-hook #'go-jira-popup--maybe-hide))
    (remove-hook 'post-command-hook #'go-jira-popup--update t)
    (when (bound-and-true-p evil-mode)
      (remove-hook 'evil-insert-state-entry-hook #'go-jira-popup--hide t))
    (remove-hook 'buffer-list-update-hook #'go-jira-popup--maybe-hide)
    (go-jira-popup--hide)))

;;;###autoload
(defun go-jira-popup-enable ()
  "Enable go-jira-popup-mode in current buffer."
  (interactive)
  (go-jira-popup-mode 1))

;;;###autoload
(defun go-jira-enable-popup+eldoc ()
  "Enable both go-jira-eldoc-mode and go-jira-popup-mode in current buffer.
In GUI Emacs with posframe, the popup will be shown.
In terminal or without posframe, eldoc provides fallback."
  (interactive)
  (go-jira-eldoc-enable)
  (go-jira-popup-enable))

(provide 'go-jira-eldoc)
;;; go-jira-eldoc.el ends here
