;;; translate-popup.el --- Popup for google-translate in-place -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: January 08, 2026
;; Modified: January 08, 2026
;; Version: 0.0.1
;; Keywords: convenience text tools
;; Homepage: https://github.com/agzam/translate-popup
;; Package-Requires: ((emacs "30.2"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'google-translate-core)
(require 'google-translate-core-ui)
(require 'google-translate-default-ui)
(require 'thingatpt)
(require 'evil-states)
(require 'posframe)

;;; Variables

(defvar translate-popup-cache (make-hash-table :test 'equal)
  "Cache for translations to avoid repeated API calls.
Keys are (source-lang . target-lang . text),
values are (timestamp . translation).")

(defvar translate-popup-cache-ttl 3600
  "Time-to-live for cache entries in seconds (default: 1 hour).")

(defvar translate-popup-max-width 80
  "Maximum width for popup translation text.")

(defvar translate-popup-buffer " *translate-popup*"
  "Buffer name for translation popup.")

(defcustom translate-popup-idle-delay 0.5
  "Number of seconds to wait before fetching translation.
Only fetch after cursor has been stable for this duration."
  :type 'number
  :group 'google-translate-core-ui)

(defcustom translate-popup-max-paragraph-length 1000
  "Maximum length of paragraph to translate automatically.
Paragraphs longer than this won't trigger automatic translation."
  :type 'integer
  :group 'google-translate-core-ui)

(defcustom translate-popup-poshandler #'posframe-poshandler-window-bottom-right-corner
  "Poshandler function for positioning the translation popup.
Common options:
  - `posframe-poshandler-window-bottom-right-corner' -
      default, doesn't block text
  - `posframe-poshandler-point-bottom-left-corner' - below cursor
  - `posframe-poshandler-frame-center' (center of frame)"
  :type 'function
  :group 'google-translate-core-ui)

(defvar-local translate-popup--pending-timer nil
  "Timer for pending translation fetch request.")

(defvar-local translate-popup--last-text nil
  "Last text we showed a popup for.")

(defvar-local translate-popup--oneshot-hooks-active nil
  "Whether one-shot auto-dismiss hooks are currently active.")

(defvar translate-popup--posframe-available-p nil
  "Whether posframe is available.")

(setq translate-popup--posframe-available-p
      (and (display-graphic-p)
           (require 'posframe nil t)))

(defface translate-popup-face
  '((t :inherit default))
  "Face for translation popup."
  :group 'google-translate-core-ui)

(defface translate-popup-border-face
  '((t :inherit font-lock-comment-face))
  "Face for popup border."
  :group 'google-translate-core-ui)

(defface translate-popup-footer-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for popup footer showing translation direction."
  :group 'google-translate-core-ui)

;;; Cache functions

(defun translate-popup--cache-key (source-lang target-lang text)
  "Create cache key from SOURCE-LANG, TARGET-LANG, and TEXT."
  (cons source-lang (cons target-lang text)))

(defun translate-popup--cache-get (source-lang target-lang text)
  "Get TEXT translation from cache if valid.
For SOURCE-LANG to TARGET-LANG"
  (when-let* ((key (translate-popup--cache-key source-lang target-lang text))
              (entry (gethash key translate-popup-cache))
              (timestamp (car entry))
              (translation (cdr entry)))
    (if (< (- (float-time) timestamp) translate-popup-cache-ttl)
        translation
      (remhash key translate-popup-cache)
      nil)))

(defun translate-popup--cache-put (source-lang target-lang text translation)
  "Store TRANSLATION for TEXT in cache with current timestamp.
from SOURCE-LANG to TARGET-LANG."
  (let ((key (translate-popup--cache-key source-lang target-lang text)))
    (puthash key (cons (float-time) translation) translate-popup-cache)))

(defun translate-popup--clear-cache ()
  "Clear the entire translation cache."
  (interactive)
  (clrhash translate-popup-cache)
  (message "Translation popup cache cleared"))

;;; Text detection functions

(defun translate-popup--at-paragraph-boundary-p ()
  "Return non-nil if point is at the beginning or end of a paragraph.
Specifically, returns t if point is within 5 characters of a paragraph boundary."
  (save-excursion
    (let ((orig-point (point))
          (threshold 5))
      (or
       ;; Check if at paragraph start
       (progn
         (forward-paragraph -1)
         (skip-chars-forward " \t\n")
         (<= (abs (- (point) orig-point)) threshold))
       ;; Check if at paragraph end
       (progn
         (goto-char orig-point)
         (forward-paragraph 1)
         (skip-chars-backward " \t\n")
         (<= (abs (- (point) orig-point)) threshold))))))

(defun translate-popup--get-paragraph-text ()
  "Get the text of the current paragraph.
Returns nil if paragraph is too long."
  (save-excursion
    (let ((start (save-excursion
                   ;; Move forward then backward to reliably get start of current paragraph
                   ;; This handles the case when cursor is at first character
                   (forward-paragraph 1)
                   (backward-paragraph 1)
                   (point)))
          (end (save-excursion
                 (forward-paragraph 1)
                 (point))))
      (when (and start end
                 (< (- end start) translate-popup-max-paragraph-length))
        (string-trim (buffer-substring-no-properties start end))))))

(defun translate-popup--get-text-to-translate ()
  "Get text to translate based on context.
Returns text string or nil if nothing appropriate to translate.
Priority: active region > paragraph (if at boundary) > word at point."
  (cond
   ;; Active region
   ((use-region-p)
    (string-trim
     (buffer-substring-no-properties (region-beginning) (region-end))))
   ;; Paragraph if at boundary
   ((translate-popup--at-paragraph-boundary-p)
    (translate-popup--get-paragraph-text))
   ;; Word at point
   ((word-at-point)
    (substring-no-properties (word-at-point)))
   ;; Nothing to translate
   (t nil)))

;;; Translation fetching

(defun translate-popup--fetch-translation (source-lang target-lang text callback)
  "Fetch translation for TEXT from SOURCE-LANG to TARGET-LANG.
Call CALLBACK with (text translation) when done.
Returns immediately if found in cache."
  (if-let ((cached (translate-popup--cache-get source-lang target-lang text)))
      (funcall callback text cached)
    ;; Fetch from Google Translate
    (condition-case err
        (when-let* ((json (google-translate-request source-lang target-lang text))
                    (translation (google-translate-json-translation json)))
          (translate-popup--cache-put source-lang target-lang text translation)
          (funcall callback text translation))
      (error
       (message "Translation error: %s" (error-message-string err))))))

;;; Popup display functions

(defun translate-popup--format-translation (_text translation source-lang target-lang)
  "Format TRANSLATION with footer showing translation direction.
TEXT is the source text, SOURCE-LANG and TARGET-LANG are language codes."
  (let* ((direction-footer
          (propertize
           (format "\n\n%s → %s" source-lang target-lang)
           'face 'translate-popup-footer-face))
         (full-text (concat translation direction-footer)))
    (with-temp-buffer
      (insert full-text)
      (fill-region (point-min) (point-max) translate-popup-max-width)
      (buffer-string))))

(defun translate-popup--hide ()
  "Hide the translation popup."
  (when translate-popup--posframe-available-p
    (posframe-hide translate-popup-buffer))
  (setq translate-popup--last-text nil)
  (translate-popup--remove-oneshot-hooks))

(defun translate-popup--cancel-pending ()
  "Cancel any pending fetch timer."
  (when translate-popup--pending-timer
    (cancel-timer translate-popup--pending-timer)
    (setq translate-popup--pending-timer nil)))

(defun translate-popup--oneshot-dismiss ()
  "One-shot hook function to auto-dismiss popup on next command.
Automatically removes itself after running."
  ;; Skip the first call (the command that showed the popup)
  ;; Only dismiss on subsequent commands
  (if (eq translate-popup--oneshot-hooks-active 'first-run)
      (setq translate-popup--oneshot-hooks-active t)
    (translate-popup--hide)))

(defun translate-popup--add-oneshot-hooks ()
  "Add one-shot hooks to auto-dismiss popup on next user action.
Used when popup is shown via explicit commands (not minor mode)."
  (unless (or translate-popup-mode translate-popup--oneshot-hooks-active)
    ;; Mark as 'first-run so the hook skips the current command
    (setq translate-popup--oneshot-hooks-active 'first-run)
    (add-hook 'post-command-hook #'translate-popup--oneshot-dismiss nil t)
    (when (bound-and-true-p evil-mode)
      (add-hook 'evil-insert-state-entry-hook #'translate-popup--oneshot-dismiss nil t))))

(defun translate-popup--remove-oneshot-hooks ()
  "Remove one-shot auto-dismiss hooks."
  (when (and translate-popup--oneshot-hooks-active
             (not (eq translate-popup--oneshot-hooks-active nil)))
    (setq translate-popup--oneshot-hooks-active nil)
    (remove-hook 'post-command-hook #'translate-popup--oneshot-dismiss t)
    (when (bound-and-true-p evil-mode)
      (remove-hook 'evil-insert-state-entry-hook #'translate-popup--oneshot-dismiss t))))

(defun translate-popup--maybe-hide ()
  "Hide popup if it's visible but we're not in the source buffer anymore."
  (when (and translate-popup--posframe-available-p
             (posframe-workable-p)
             (get-buffer translate-popup-buffer))
    (let* ((popup-buf (get-buffer translate-popup-buffer))
           (frame (buffer-local-value 'posframe--frame popup-buf)))
      (when (and frame (frame-live-p frame) (frame-visible-p frame))
        (let ((source-buf (buffer-local-value 'translate-popup--source-buffer popup-buf)))
          (when (or (not (eq (current-buffer) source-buf))
                    (not (get-buffer-window source-buf)))
            (translate-popup--hide)))))))

(defun translate-popup--show (text translation source-lang target-lang)
  "Show TRANSLATION for TEXT in a posframe popup with language direction."
  (when translate-popup--posframe-available-p
    (let ((formatted-text (translate-popup--format-translation
                           text translation source-lang target-lang))
          (current-buf (current-buffer)))
      (posframe-show
       translate-popup-buffer
       :string formatted-text
       :position (point)
       :poshandler translate-popup-poshandler
       :border-width 1
       :border-color (face-foreground 'translate-popup-border-face nil t)
       :background-color (face-background 'tooltip nil t)
       :foreground-color (face-foreground 'tooltip nil t)
       :internal-border-width 12
       :internal-border-color (face-background 'tooltip nil t)
       :left-fringe 8
       :right-fringe 8
       :override-parameters '((no-accept-focus . t)))
      ;; Store which buffer this popup belongs to
      (with-current-buffer translate-popup-buffer
        (setq-local translate-popup--source-buffer current-buf)))))

(defun translate-popup--update ()
  "Update translation popup for text at point."
  (if-let* ((text (translate-popup--get-text-to-translate))
            ;; Only show in normal state (evil) or when not actively typing
            (_ (or (not (bound-and-true-p evil-mode))
                   (evil-normal-state-p)))
            ;; Get language settings
            (source-lang google-translate-default-source-language)
            (target-lang google-translate-default-target-language))
      (progn
        ;; Check if this is different text than what we're showing
        (if (equal text translate-popup--last-text)
            ;; Same text - do nothing, keep showing popup
            nil
          ;; Different text - cancel pending and fetch new translation
          (translate-popup--cancel-pending)
          ;; Mark this text as current IMMEDIATELY to prevent re-triggering
          (setq translate-popup--last-text text)
          ;; Try to get from cache immediately
          (if-let ((translation (translate-popup--cache-get source-lang target-lang text)))
              (translate-popup--show text translation source-lang target-lang)
            ;; Debounced fetch: wait for cursor to be stable
            (let ((current-buf (current-buffer))
                  (current-pos (point)))
              (setq translate-popup--pending-timer
                    (run-with-idle-timer
                     translate-popup-idle-delay nil
                     (lambda ()
                       ;; Only fetch if still in same buffer/position
                       (when (and (buffer-live-p current-buf)
                                  (eq current-buf (current-buffer))
                                  (= current-pos (point)))
                         (translate-popup--fetch-translation
                          source-lang target-lang text
                          (lambda (txt trans)
                            ;; Only show if still on same text
                            (when (and (buffer-live-p current-buf)
                                       (eq current-buf (current-buffer))
                                       (equal txt (translate-popup--get-text-to-translate)))
                              (translate-popup--show txt trans source-lang target-lang))))))))))))
    ;; Not on translatable text, cancel and hide popup
    (translate-popup--cancel-pending)
    (translate-popup--hide)))

;;; Public API

;;;###autoload
(define-minor-mode translate-popup-mode
  "Minor mode to show Google Translate translations in posframe popups.
Automatically translates text at point based on context:
- Active region (if selected)
- Current paragraph (if at beginning or end)
- Word at point (fallback)

Uses current google-translate language settings."
  :global nil
  :lighter " TR"
  (if translate-popup-mode
      (progn
        (unless translate-popup--posframe-available-p
          (message "translate-popup-mode requires posframe in GUI Emacs")
          (setq translate-popup-mode nil))
        (when translate-popup-mode
          (add-hook 'post-command-hook #'translate-popup--update nil t)
          ;; Clean up when entering insert mode
          (when (bound-and-true-p evil-mode)
            (add-hook 'evil-insert-state-entry-hook #'translate-popup--hide nil t))
          ;; Hide popup when switching buffers/tabs/windows (global hook)
          (add-hook 'buffer-list-update-hook #'translate-popup--maybe-hide)))
    (remove-hook 'post-command-hook #'translate-popup--update t)
    (when (bound-and-true-p evil-mode)
      (remove-hook 'evil-insert-state-entry-hook #'translate-popup--hide t))
    (remove-hook 'buffer-list-update-hook #'translate-popup--maybe-hide)
    (translate-popup--cancel-pending)
    (translate-popup--hide)))

;;;###autoload
(defun translate-popup-enable ()
  "Enable `translate-popup-mode' in current buffer."
  (interactive)
  (translate-popup-mode 1))

;;;###autoload
(defun translate-popup-disable ()
  "Disable `translate-popup-mode' in current buffer."
  (interactive)
  (translate-popup-mode -1))

;;; Explicit translation commands

;;;###autoload
(defun translate-popup-translate-at-point ()
  "Show translation popup for text at point.
Works even when `translate-popup-mode' is not enabled.
Translates region, paragraph (if at boundary), or word at point.
Auto-dismisses on next command/movement."
  (interactive)
  (when-let* ((text (translate-popup--get-text-to-translate))
              (source-lang google-translate-default-source-language)
              (target-lang google-translate-default-target-language))
    (translate-popup--fetch-translation
     source-lang target-lang text
     (lambda (txt trans)
       (translate-popup--show txt trans source-lang target-lang)
       (translate-popup--add-oneshot-hooks)))))

;;;###autoload
(defun translate-popup-translate-region (start end)
  "Show translation popup for region between START and END.
Auto-dismisses on next command/movement."
  (interactive "r")
  (let* ((text (string-trim (buffer-substring-no-properties start end)))
         (source-lang google-translate-default-source-language)
         (target-lang google-translate-default-target-language))
    (when (not (string-empty-p text))
      (translate-popup--fetch-translation
       source-lang target-lang text
       (lambda (txt trans)
         (translate-popup--show txt trans source-lang target-lang)
         (translate-popup--add-oneshot-hooks))))))

;;;###autoload
(defun translate-popup-translate-paragraph ()
  "Show translation popup for current paragraph.
Auto-dismisses on next command/movement."
  (interactive)
  (when-let* ((text (translate-popup--get-paragraph-text))
              (source-lang google-translate-default-source-language)
              (target-lang google-translate-default-target-language))
    (translate-popup--fetch-translation
     source-lang target-lang text
     (lambda (txt trans)
       (translate-popup--show txt trans source-lang target-lang)
       (translate-popup--add-oneshot-hooks)))))

;;;###autoload
(defun translate-popup-translate-word ()
  "Show translation popup for word at point.
Auto-dismisses on next command/movement."
  (interactive)
  (when-let* ((text (and (word-at-point)
                         (substring-no-properties (word-at-point))))
              (source-lang google-translate-default-source-language)
              (target-lang google-translate-default-target-language))
    (translate-popup--fetch-translation
     source-lang target-lang text
     (lambda (txt trans)
       (translate-popup--show txt trans source-lang target-lang)
       (translate-popup--add-oneshot-hooks)))))

;;;###autoload
(defun translate-popup-dismiss ()
  "Dismiss the translation popup."
  (interactive)
  (translate-popup--hide))

(provide 'translate-popup)
;;; translate-popup.el ends here
