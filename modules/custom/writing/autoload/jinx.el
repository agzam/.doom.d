;;; custom/writing/autoload/jinx.el -*- lexical-binding: t; -*-

(defvar jinx-autocorrect--suggestions nil)
(defvar jinx-autocorrect--ts nil)
(defvar jinx-autocorrect--pos nil)

;;;###autoload
(defun jinx-autocorrect-last+ (&optional prompt)
  "Autocorrect previous misspelling. If called repeatedly, it cycles
through word suggestions unless the last call happened a while
ago. With a prefix argument opens `jinx-correct-word' dialog."
  (interactive "P")
  (save-excursion
    (let* ((now (current-time))
           (diff (when jinx-autocorrect--ts
                   (float-time (time-subtract now jinx-autocorrect--ts)))))
      (if (and jinx-autocorrect--pos
               jinx-autocorrect--suggestions
               ;; last correction happened no longer than N seconds ago
               (< diff 5))
          (let* ((_ (goto-char jinx-autocorrect--pos))
                 (cur-word (word-at-point))
                 (word-end (+ jinx-autocorrect--pos (length cur-word))))
            (if prompt
                (jinx--correct-guard
                 (jinx-correct-word (point) word-end)
                 (setq jinx-autocorrect--ts nil
                       jinx-autocorrect--pos nil))
              (let* ((new-word (ring-next
                                jinx-autocorrect--suggestions
                                cur-word))
                     (next-sugs (mapconcat
                                 #'identity
                                 (thread-last
                                   jinx-autocorrect--suggestions
                                   ring-elements
                                   (seq-drop-while
                                    (lambda (x)
                                      (not (string= x new-word))))
                                   cdr-safe)
                                 ", ")))
                (message next-sugs)
                (delete-region jinx-autocorrect--pos word-end)
                (insert-before-markers new-word)
                (setq jinx-autocorrect--ts now)))
            (undo-auto-amalgamate))
        (let* ((zone-end (point))
               ;; fix only within last N sentences
               (zone-beg (progn (backward-sentence 2)
                                (point)))
               ;; pick the last overlay (last misspelling)
               (ov (car-safe
                    (last (jinx--force-overlays
                           zone-beg zone-end :visible t)))))
          (if prompt
              ;; if called with argument, open the dialog
              (jinx--correct-guard
               (jinx--correct-overlay ov)
               (setq jinx-autocorrect--ts nil
                     jinx-autocorrect--pos nil))
            (let* ((pos-beg (when ov (overlay-start ov)))
                   (word (when ov
                           (buffer-substring-no-properties
                            (overlay-start ov)
                            (overlay-end ov))))
                   (sugs (when word
                           (thread-last
                             word jinx--correct-suggestions
                             (seq-map #'substring-no-properties)
                             (seq-remove
                              (lambda (x)
                                (or (string-prefix-p "@" x)
                                    (string-prefix-p "+" x)
                                    (string-prefix-p "*" x)))))))
                   (next-sugs (mapconcat #'identity (cdr-safe sugs) ", ")))
              (message next-sugs)
              (jinx--correct-guard
               (jinx--correct-replace ov (car sugs)))
              (setq jinx-autocorrect--suggestions
                    (ring-convert-sequence-to-ring sugs))
              (setq jinx-autocorrect--ts (current-time))
              (setq jinx-autocorrect--pos pos-beg)))
          (undo-auto-amalgamate))))))

;;;###autoload
(defun insert-comma ()
  "Cleverly insert comma."
  (interactive)
  (cond
   ;; I don't want char-equal to fail when (char-before) returns nil
   ;; so we use 'default char' #x1F436 which represents 🐶
   ((char-equal ?\s (or (char-before) #x1F436))
    (progn
      (re-search-backward "\\>") ; find the previous word boundary
      (insert ",")
      (forward-char)))
   ((char-equal ?\s (or (char-after) #x1F436))
    (progn
      (insert ",")
      (forward-char)))
   (t (insert ", "))))
