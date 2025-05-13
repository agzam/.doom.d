;;; custom/web-browsing/autoload/subed.el -*- lexical-binding: t; -*-
(defvar-local subed--srt-metadata-hidden nil
  "Whether SRT metadata is currently hidden.")

;;;###autoload
(defun subed-toggle-srt-metadata ()
  "Hide timestamps behind overlays in a .srt file."
  (interactive)
  (if subed--srt-metadata-hidden
      (remove-overlays nil nil 'srt-metadata t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9]+\n[0-9:.,-]+ --> [0-9:.,-]+\\)\n\\(.*\\(?:\n.*\\)*?\\)\n\n" nil t)
        (let ((ov-metadata (make-overlay (match-beginning 1) (match-beginning 2)))
              (ov-extra-newline (make-overlay (1+ (match-end 2)) (match-end 0))))
          (overlay-put ov-metadata 'invisible t)
          (overlay-put ov-metadata 'srt-metadata t)
          (overlay-put ov-extra-newline 'invisible t)
          (overlay-put ov-extra-newline 'srt-metadata t)))))
  (setq subed--srt-metadata-hidden (not subed--srt-metadata-hidden))
  (redraw-display))

;;;;;###autoload
(defun subed-view-plain-text ()
  "Show only subtitle text (without timestamp metadata) in a separate buffer."
  (interactive)
  (let ((text-content "")
        (buf (get-buffer-create "*Subtitle Text Only*")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[0-9]+\n[0-9:.,-]+ --> [0-9:.,-]+\n\\(.*\\(?:\n.*\\)*?\\)\n\n" nil t)
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
