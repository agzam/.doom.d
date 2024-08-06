;;; custom/ai/autoload/piper.el -*- lexical-binding: t; -*-
(defvar piper-voice-model
  (if (featurep :system 'macos)
      "en_US-hfc_female-medium.onnx"
    "/usr/share/piper-voices/en/en_US/hfc_female/medium/en_US-hfc_female-medium.onnx"))

(defvar piper-player-command
  (if (featurep :system 'macos)
      "sox -t raw -r 22050 -b 16 -e signed-integer -c 1 - -d"
    "aplay -r 22050 -f S16_LE -t raw -"))

;;;###autoload
(defun buffer->piper-tts (buffer &optional model)
  "Sends given buffer content to piper-tts."
  (let* ((model (or model piper-voice-model))
         (piper (executable-find "piper-tts"))
         (tmp (make-temp-file "tts-input-"))
         (_ (with-temp-file tmp
              (insert-buffer-substring buffer))))
    (unless piper (user-error "piper-tts executable not found."))
    (when (get-process "tts")
      (kill-process (get-process "tts")))
    (start-process
     "tts" nil
     "sh" "-c"
     (format
      (concat "cat %s | %s --model %s "
              "--length_scale 0.7 "
              "--sentence_silence 0.1 "
              "--output_raw | %s")
      (shell-quote-argument tmp)
      piper model piper-player-command))
    (run-with-timer 2 nil (lambda (tfile) (delete-file tfile)) tmp)))

;;;###autoload
(defun text->tts (&optional buffer->tts-fn)
  "Sends selected text (or buffer content) tts."
  (interactive)
  (let* ((content (if (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
    (with-temp-buffer
      (insert content)
      (funcall (or buffer->tts-fn #'buffer->piper-tts)
               (current-buffer)))))

;;;###autoload
(defun clipboard->tts ()
  "Sends clipboard content to tts."
  (interactive)
  (with-temp-buffer
    (insert (shell-command-to-string "xsel"))
    (text->tts)))
