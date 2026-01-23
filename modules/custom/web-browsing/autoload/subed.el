;;; custom/web-browsing/autoload/subed.el -*- lexical-binding: t; -*-
(defvar-local subed--subtitle-metadata-hidden nil
  "Whether subtitle metadata is currently hidden.")

;;;###autoload
(defun subed-toggle-srt-metadata ()
  "Hide timestamps behind overlays in subtitle files (.srt or .vtt)."
  (interactive)
  (if subed--subtitle-metadata-hidden
      (remove-overlays nil nil 'subtitle-metadata t)
    (save-excursion
      (goto-char (point-min))
      ;; Skip VTT header if present
      (when (looking-at "WEBVTT")
        (forward-line 1)
        (while (looking-at "^\\(NOTE\\|Kind:\\|Language:\\)")
          (forward-line 1))
        (when (looking-at "^$")
          (forward-line 1)))
      ;; Match both SRT and VTT subtitle entries
      ;; SRT: number\ntimestamp --> timestamp\ntext\n\n
      ;; VTT with ID: id\ntimestamp --> timestamp\ntext\n\n
      ;; VTT without ID: timestamp --> timestamp\ntext\n\n
      (while (re-search-forward "^\\(\\(?:[0-9]+\n\\)?\\([0-9:.,-]+ --> [0-9:.,-]+\\(?: .*\\)?\\)\\)\n\\(.*\\(?:\n.*\\)*?\\)\n\n" nil t)
        (let ((ov-metadata (make-overlay (match-beginning 1) (match-beginning 3)))
              (ov-extra-newline (make-overlay (1+ (match-end 3)) (match-end 0))))
          (overlay-put ov-metadata 'invisible t)
          (overlay-put ov-metadata 'subtitle-metadata t)
          (overlay-put ov-extra-newline 'invisible t)
          (overlay-put ov-extra-newline 'subtitle-metadata t)))))
  (setq subed--subtitle-metadata-hidden (not subed--subtitle-metadata-hidden))
  (redraw-display))

;;;;;###autoload
(defun subed-view-plain-text ()
  "Show only subtitle text (without timestamp metadata) in a separate buffer.
Works with both .srt and .vtt files."
  (interactive)
  (let ((text-content "")
        (buf (get-buffer-create "*Subtitle Text Only*")))
    (save-excursion
      (goto-char (point-min))
      ;; Skip VTT header if present
      (when (looking-at "WEBVTT")
        (forward-line 1)
        (while (looking-at "^\\(NOTE\\|Kind:\\|Language:\\)")
          (forward-line 1))
        (when (looking-at "^$")
          (forward-line 1)))
      ;; Match both SRT and VTT subtitle entries
      ;; SRT: number\ntimestamp --> timestamp\ntext\n\n
      ;; VTT with ID: id\ntimestamp --> timestamp\ntext\n\n
      ;; VTT without ID: timestamp --> timestamp\ntext\n\n
      (while (re-search-forward "^\\(?:[0-9]+\n\\)?[0-9:.,-]+ --> [0-9:.,-]+\\(?: .*\\)?\n\\(.*\\(?:\n.*\\)*?\\)\n\n" nil t)
        (let ((subtitle-text (match-string 1)))
          ;; Add spacing between subtitles but avoid consecutive empty lines
          (when (> (length text-content) 0)
            (setq text-content (concat text-content "\n")))
          (setq text-content (concat text-content subtitle-text)))))
    (with-current-buffer buf
      (erase-buffer)
      (insert text-content)
      ;; Clean up any remaining consecutive empty lines
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match "\n\n"))
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))


;;;###autoload
(defun subed-mpv-play-from-file+ (&optional file)
  (interactive)
  (let* ((current-file (buffer-file-name))
         (directory (file-name-directory current-file))
         (base-name (file-name-base current-file))
         (mp3-file (concat directory base-name ".mp3")))
    (if (and current-file
             (string= (file-name-extension current-file) "srt")
             (file-exists-p mp3-file))
        (subed-mpv-play-from-file mp3-file)
      (subed-mpv-play-from-file file))
    (subed-mpv-unpause)))
