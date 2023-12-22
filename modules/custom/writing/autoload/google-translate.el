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

;;;###autoload
(defun +google-translate-es->en ()
  (interactive)
  (let ((google-translate-default-source-language "es")
        (google-translate-default-target-language "en"))
    (google-translate-query-translate)))

;;;###autoload
(defun +google-translate-en->es ()
  (interactive)
  (let ((google-translate-default-source-language "en")
        (google-translate-default-target-language "es"))
    (google-translate-query-translate)))


;;;###autoload
(defun number-to-words (number)
  "Convert a number into words using Node.js."
  (when (with-temp-buffer
          (not (= 0 (call-process
                     "node" nil t nil "-e"
                     "try{require('number-to-words')}catch(e){process.exit(1)}"))))
    (shell-command "npm install number-to-words"))
  (let ((output
         (shell-command-to-string
          (format
           "node -e \"var toWords = require('number-to-words'); console.log(toWords.toWords(%d));\""
           number))))
    (replace-regexp-in-string "\n" "" output)))

(defadvice! google-translate-years-to-words-a
  (orig-fn src-lang tgt-lang text &optional output-dest)
  "Google Translate doesn't spell out numbers and I often need to
see/hear them in their written form. For example, I don't know
how to say \"2023\" in Spanish. This function advises g-translate,
so text that contains something that looks like a year (four
digits), would be converted to a written representation, so a
text like: \"2023 was a better year than 2021\" would translate to:
\"dos mil veintitrés fue un año mejor que dos mil veintiuno\""
  :around #'google-translate-translate
  (if (string= src-lang "en")
      (let ((txt (replace-regexp-in-string
                  "\\b\\([0-9]\\{4\\}\\)\\b"
                  (lambda (match)
                    (replace-regexp-in-string
                     ", " " and "
                     (number-to-words (string-to-number match))))
                  text)))
        (funcall orig-fn src-lang tgt-lang txt output-dest))
    (funcall orig-fn src-lang tgt-lang text output-dest)))
