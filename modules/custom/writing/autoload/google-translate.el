;;; custom/writing/autoload/google-translate.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'set-google-translate-languages "custom/writing/autoload/google-translate" nil t)
(defun set-google-translate-languages (&optional override-p)
  "Set source language for google translate.
For instance pass En as source for English."
  (interactive "P")
  (autoload 'google-translate-read-args "google-translate-default-ui")
  (let* ((langs (google-translate-read-args override-p nil))
         (source-language (car langs))
         (target-language (cadr langs)))
    (setq google-translate-default-source-language source-language)
    (setq google-translate-default-target-language target-language)
    (message
     (format "Set google translate source language to %s and target to %s"
             source-language target-language))))

;;;###autoload (autoload 'set-google-translate-language "custom/writing/autoload/google-translate" nil t)
(defun set-google-translate-target-language ()
        "Set the target language for google translate."
        (interactive)
        (set-google-translate-languages nil))
