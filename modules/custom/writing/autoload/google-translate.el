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
    (shell-command "npm -g install number-to-words"))
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
                  "\\b\\([0-9]\\{3,4\\}\\)\\b"
                  (lambda (match)
                    (replace-regexp-in-string
                     ", " " and "
                     (number-to-words (string-to-number match))))
                  text)))
        (funcall orig-fn src-lang tgt-lang txt output-dest))
    (funcall orig-fn src-lang tgt-lang text output-dest)))

(defun translate--set-lang (type lang)
  (interactive)
  (set (if (eq type :source)
           'google-translate-default-source-language
         'google-translate-default-target-language)
       lang)
  (setq google-translate-default-target-language
         (if (eq type :source)
             (pcase lang
               ("en" "es")
               ("ru" "en")
               ("es" "en"))
           lang)))

(defvar translate--last-context nil)
(defvar translate--minibuffer-text nil)
(defvar translate--langs-src (ring-convert-sequence-to-ring '("en" "es" "ru")))
(defvar translate--langs-target nil)

(transient-define-suffix translate--set-source ()
  :description "source"
  :key "s"
  :variable 'google-translate-default-source-language
  :class 'transient-lisp-variable
  (interactive)
  (let* ((cur (oref (transient-suffix-object) value))
         (nxt (ring-next translate--langs-src cur)))
    (translate--set-lang :source nxt)
    (oset (transient-suffix-object) value nxt)
    (setq translate--langs-target
          (thread-last
            translate--langs-src
            cddr
            (seq-remove (lambda (x) (string= nxt x)))
            ring-convert-sequence-to-ring))
    (translate--set-target google-translate-default-target-language)
    (transient-reset)))

(transient-define-suffix translate--set-target (&optional explicit-val)
  :description "target"
  :key "t"
  :variable 'google-translate-default-target-language
  :class 'transient-lisp-variable
  (interactive)
  (if explicit-val
      (oset (transient-suffix-object) value explicit-val)
    (let* ((cur (oref (transient-suffix-object) value))
           (nxt (ring-next translate--langs-target cur)))
      (translate--set-lang :target nxt)
      (oset (transient-suffix-object) value nxt))))

(transient-define-infix translate--minibuffer ()
  "Type some text in the minibuffer."
  :description (lambda ()
                 (if translate--minibuffer-text
                     (format "Text: %s" translate--minibuffer-text)
                   "Type some text"))
  :variable 'translate--minibuffer-text
  :class 'transient-lisp-variable
  :format "%k %d"
  :key "i"
  :reader (lambda (&rest _)
            (let* ((src google-translate-default-source-language)
                   (tgt google-translate-default-target-language)
                   (current-input-method
                    (symbol-name
                     (google-translate-find-preferable-input-method
                      src))))
              (read-string
               (format "Translate. %s -> %s: " src tgt)
               nil nil nil t))))

(defun translate--translate ()
  (interactive)
  (let ((content (cond ((use-region-p)
                        (buffer-substring
                         (region-beginning)
                         (region-end)))
                       (translate--minibuffer-text
                        translate--minibuffer-text)
                       (t (word-at-point)))))
    (with-temp-buffer
      (insert content)
      (setq translate--last-context
            (buffer-substring-no-properties (point-min) (point-max)))
      (google-translate-buffer))))

;;;###autoload
(transient-define-prefix translate-transient ()
  "Translate"
  [["Langs"
    (translate--set-source)
    (translate--set-target)]
   [""
    ("r" "reverse" (lambda ()
                     (interactive)
                     (cl-rotatef
                      google-translate-default-target-language
                      google-translate-default-source-language)
                     (transient-reset))
     :transient t)
    (translate--minibuffer)]
   ["" ("RET" "Go!" translate--translate)]]
  [:hide always
   [("<return>" "Go!" translate--translate)]]
  (interactive)
  (setq translate--minibuffer-text nil)
  (transient-setup 'translate-transient))
