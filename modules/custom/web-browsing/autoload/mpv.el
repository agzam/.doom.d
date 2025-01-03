;;; custom/web-browsing/autoload/mpv.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mpv-speed-reset ()
  (interactive)
  (mpv-speed-set 1))

;;;###autoload
(defun mpv-open+ (&optional path)
  (interactive)
  (let* ((url-regex "\\`https?://")
         (path (or path
                   (cond
                    ((and (car kill-ring)
                          (string-match url-regex (car kill-ring)))
                     (car kill-ring))

                    ((and (thing-at-point 'word)
                          (string-match url-regex (thing-at-point 'word)))
                     (thing-at-point 'word))

                    ((eq major-mode 'org-mode)
                     (org-element-property :path (org-element-context)))))))
    (cond
     ((eq major-mode 'dired-mode)
      (mpv-play (dired-get-file-for-visit)))

     ((and (eq major-mode 'org-mode) path)
      (mpv-play path))

     (t (mpv-play-url (read-string "Play: " path))))))

(defvar mpv--osc-style "auto")
(defvar mpv--subtitle-visible "auto")

;;;###autoload
(defun mpv-toggle-osc ()
  (interactive)
  (mpv-run-command
   "script-message" "osc-visibility"
   (setf mpv--osc-style (if (string= mpv--osc-style "auto")
                            "always" "auto"))))

;;;###autoload
(defun mpv-toggle-subtitles ()
  (interactive)
  (mpv-run-command
   "set" "sub-visibility"
   (setq mpv--subtitle-visible
         (if (string-match-p "yes\\|auto" mpv--subtitle-visible)
             "no" "yes"))))

;;;###autoload
(transient-define-prefix mpv-transient ()
  "mpv"
  ["mpv"
   [("f" "follow" elfeed-tube-mpv-follow-mode)
    ("w" "where" elfeed-tube-mpv-where)]
   [("k" "vol up" mpv-volume-increase :transient t)
    ("j" "vol down" mpv-volume-decrease :transient t)]
   [("p" "prev" mpv-playlist-prev :transient t)
    ("n" "next" mpv-playlist-next :transient t)]
   [("h" "<<" mpv-seek-backward :transient t)
    ("l" ">>" mpv-seek-forward :transient t)]
   [("i" "osc" mpv-toggle-osc :transient t)
    ("c" "subs" mpv-toggle-subtitles :transient t)]
   [("," "slower" mpv-speed-decrease :transient t)
    ("." "faster" mpv-speed-increase :transient t)
    ("0" "reset" mpv-speed-reset)]
   [("SPC" "pause" mpv-pause :transient t)
    ("o" "play" mpv-open+ :transient t)
    ("Q" "quit" mpv-kill)]])
