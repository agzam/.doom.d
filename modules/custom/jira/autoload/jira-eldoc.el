;;; custom/jira/autoload/jira-eldoc.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Eldoc integration to show Jira ticket descriptions on hover in normal mode
;; Also provides posframe popup support to display descriptions in GUI Emacs

;;; Code:

(defvar jira-eldoc-cache (make-hash-table :test 'equal)
  "Cache for Jira ticket descriptions to avoid repeated API calls.")

(defvar jira-eldoc-cache-ttl 3600
  "Time-to-live for cache entries in seconds (default: 1 hour).")

(defvar jira-popup-max-width 80
  "Maximum width for popup description text.")

(defvar jira-popup-buffer " *jira-ticket-popup*"
  "Buffer name for Jira ticket popup.")

(defface jira-popup-face
  '((t :inherit default))
  "Face for Jira ticket description popup."
  :group 'jira)

(defface jira-popup-border-face
  '((t :inherit font-lock-comment-face))
  "Face for Jira popup border."
  :group 'jira)

(defvar-local jira--last-ticket-pos nil
  "Last position where we showed a Jira ticket popup.")

(defvar jira--posframe-available-p nil
  "Whether posframe is available.")

(setq jira--posframe-available-p
      (and (display-graphic-p)
           (require 'posframe nil t)))

(defun jira-eldoc--cache-get (ticket)
  "Get TICKET description from cache if valid."
  (when-let* ((entry (gethash ticket jira-eldoc-cache))
              (timestamp (car entry))
              (description (cdr entry)))
    (if (< (- (float-time) timestamp) jira-eldoc-cache-ttl)
        description
      (remhash ticket jira-eldoc-cache)
      nil)))

(defun jira-eldoc--cache-put (ticket description)
  "Store TICKET DESCRIPTION in cache with current timestamp."
  (puthash ticket (cons (float-time) description) jira-eldoc-cache))

(defun jira-eldoc--clear-cache ()
  "Clear the entire Jira eldoc cache."
  (interactive)
  (clrhash jira-eldoc-cache)
  (message "Jira eldoc cache cleared"))

(defun jira-eldoc--fetch-description (ticket)
  "Fetch description for TICKET, using cache when available."
  (or (jira-eldoc--cache-get ticket)
      (when-let ((summary (condition-case nil
                              (jira-summary ticket)
                            (error nil))))
        (jira-eldoc--cache-put ticket summary)
        summary)))

(defun jira-eldoc--ticket-at-point ()
  "Return Jira ticket at point if present, nil otherwise."
  (when-let* ((ticket-pattern "\\b[A-Z]\\{2,10\\}-[0-9]+\\b")
              (thing (thing-at-point 'symbol t)))
    (when (string-match-p (concat "\\`" ticket-pattern "\\'") thing)
      thing)))

(defun jira-eldoc--ticket-bounds-at-point ()
  "Return (TICKET . (BEG . END)) for ticket at point, or nil."
  (when-let* ((ticket-pattern "\\b[A-Z]\\{2,10\\}-[0-9]+\\b")
              (bounds (bounds-of-thing-at-point 'symbol))
              (beg (car bounds))
              (end (cdr bounds))
              (ticket (buffer-substring-no-properties beg end)))
    (when (string-match-p (concat "\\`" ticket-pattern "\\'") ticket)
      (cons ticket bounds))))

(defun jira-popup--format-description (ticket description)
  "Format TICKET and DESCRIPTION for popup display."
  (let* ((ticket-bold (propertize ticket 'face '(:weight bold)))
         (full-text (format "%s: %s" ticket-bold description))
         (max-width jira-popup-max-width))
    (with-temp-buffer
      (insert full-text)
      (fill-region (point-min) (point-max) max-width)
      (buffer-string))))

(defun jira-popup--hide ()
  "Hide the Jira ticket popup."
  (when jira--posframe-available-p
    (posframe-hide jira-popup-buffer))
  (setq jira--last-ticket-pos nil))

(defun jira-popup--maybe-hide ()
  "Hide popup if it's visible but we're not in the source buffer anymore."
  (when (and jira--posframe-available-p
             (posframe-workable-p)
             (get-buffer jira-popup-buffer)
             (frame-visible-p (buffer-local-value 'posframe--frame
                                                  (get-buffer jira-popup-buffer))))
    (let ((source-buf (buffer-local-value 'jira--popup-source-buffer
                                         (get-buffer jira-popup-buffer))))
      ;; Hide if we're not in the source buffer or source buffer isn't visible
      (when (or (not (eq (current-buffer) source-buf))
                (not (get-buffer-window source-buf)))
        (jira-popup--hide)))))

(defun jira-popup--show (ticket description)
  "Show DESCRIPTION for TICKET in a posframe popup."
  (if jira--posframe-available-p
      (let ((text (jira-popup--format-description ticket description))
            (current-buf (current-buffer)))
        (posframe-show
         jira-popup-buffer
         :string text
         :position (point)
         :poshandler #'posframe-poshandler-point-bottom-left-corner
         :border-width 1
         :border-color (face-foreground 'jira-popup-border-face nil t)
         :background-color (face-background 'default nil t)
         :foreground-color (face-foreground 'default nil t)
         :internal-border-width 12
         :internal-border-color (face-background 'default nil t)
         :left-fringe 8
         :right-fringe 8
         :override-parameters '((no-accept-focus . t)))
        ;; Store which buffer this popup belongs to
        (with-current-buffer jira-popup-buffer
          (setq-local jira--popup-source-buffer current-buf))
        (setq jira--last-ticket-pos (point)))
    ;; Fallback: do nothing, eldoc will handle it
    nil))

(defun jira-popup--update ()
  "Update Jira popup for ticket at point."
  (if-let* ((ticket-info (jira-eldoc--ticket-bounds-at-point))
            (ticket (car ticket-info))
            ;; Only show in normal state
            (_ (and (bound-and-true-p evil-mode)
                    (eq evil-state 'normal))))
      (progn
        ;; Only update if we moved to a different position
        (unless (and jira--last-ticket-pos
                     (= (point) jira--last-ticket-pos))
          ;; Try to get from cache immediately
          (if-let ((description (jira-eldoc--cache-get ticket)))
              (jira-popup--show ticket description)
            ;; Fetch async if not in cache
            (run-with-idle-timer
             0.2 nil
             (lambda ()
               (when (and (eq (current-buffer) (window-buffer))
                          (jira-eldoc--ticket-at-point))
                 (when-let ((desc (jira-eldoc--fetch-description ticket)))
                   (jira-popup--show ticket desc))))))))
    ;; Not on a ticket, hide popup
    (jira-popup--hide)))

;;;###autoload
(defun jira-eldoc-function (callback &rest _ignored)
  "Eldoc documentation function for Jira tickets.
Calls CALLBACK with the ticket description when point is on a Jira ticket.
Designed to work with eldoc-documentation-functions."
  (when-let* ((ticket (jira-eldoc--ticket-at-point))
              ;; Only fetch if in normal state (not while typing)
              (_ (and (bound-and-true-p evil-mode)
                      (eq evil-state 'normal))))
    ;; Fetch asynchronously to avoid blocking
    (run-with-idle-timer
     0.1 nil
     (lambda ()
       (when-let ((description (jira-eldoc--fetch-description ticket)))
         (funcall callback
                  (format "%s: %s" ticket description)
                  :thing ticket
                  :face 'font-lock-doc-face))))))

;;;###autoload
(define-minor-mode jira-eldoc-mode
  "Minor mode to show Jira ticket descriptions in eldoc."
  :global nil
  :lighter nil
  (if jira-eldoc-mode
      (add-hook 'eldoc-documentation-functions #'jira-eldoc-function nil t)
    (remove-hook 'eldoc-documentation-functions #'jira-eldoc-function t)))

;;;###autoload
(defun jira-eldoc-enable ()
  "Enable jira-eldoc-mode in current buffer."
  (interactive)
  (jira-eldoc-mode 1)
  (eldoc-mode 1))

;;;###autoload
(define-minor-mode jira-popup-mode
  "Minor mode to show Jira ticket descriptions in posframe popups."
  :global nil
  :lighter nil
  (if jira-popup-mode
      (progn
        (add-hook 'post-command-hook #'jira-popup--update nil t)
        ;; Clean up when entering insert mode
        (when (bound-and-true-p evil-mode)
          (add-hook 'evil-insert-state-entry-hook #'jira-popup--hide nil t))
        ;; Hide popup when switching buffers/tabs/windows (global hook)
        (add-hook 'buffer-list-update-hook #'jira-popup--maybe-hide))
    (remove-hook 'post-command-hook #'jira-popup--update t)
    (when (bound-and-true-p evil-mode)
      (remove-hook 'evil-insert-state-entry-hook #'jira-popup--hide t))
    (remove-hook 'buffer-list-update-hook #'jira-popup--maybe-hide)
    (jira-popup--hide)))

;;;###autoload
(defun jira-popup-enable ()
  "Enable jira-popup-mode in current buffer."
  (interactive)
  (jira-popup-mode 1))

;;;###autoload
(defun jira-enable-popup+eldoc ()
  "Enable both jira-eldoc-mode and jira-popup-mode in current buffer.
In GUI Emacs with posframe, the popup will be shown.
In terminal or without posframe, eldoc provides fallback."
  (interactive)
  (jira-eldoc-enable)
  (jira-popup-enable))

(provide 'jira-eldoc)
;;; jira-eldoc.el ends here
