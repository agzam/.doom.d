;;; custom/web-browsing/autoload/subed.el -*- lexical-binding: t; -*-
(defvar-local subed--srt-metadata-hidden nil
  "Whether SRT metadata is currently hidden.")

;;;###autoload
(defun subed-toggle-srt-metadata ()
  (interactive)
  (if subed--srt-metadata-hidden
      (remove-overlays nil nil 'srt-metadata t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9]+\\)\n\\([0-9:.,-]+ --> [0-9:.,-]+\\)\n\\(.*\\(?:\n.*\\)*?\\)\n\n" nil t)
        (let ((ov-metadata (make-overlay (match-beginning 1) (match-beginning 3)))
              (ov-extra-newline (make-overlay (1+ (match-end 3)) (match-end 0))))
          (overlay-put ov-metadata 'invisible t)
          (overlay-put ov-metadata 'srt-metadata t)
          (overlay-put ov-extra-newline 'invisible t)
          (overlay-put ov-extra-newline 'srt-metadata t)))))
  (setq subed--srt-metadata-hidden (not subed--srt-metadata-hidden))
  (redraw-display))


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
