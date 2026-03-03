;;; custom/org/autoload/journal.el -*- lexical-binding: t; -*-

(defvar vulpea-journal--type 'work
  "Active journal type: `work' or `personal'.
Set by journal commands, read by the dynamic template function.")

(defvar-local vulpea-journal--buffer-type nil
  "Buffer-local journal type, set when visiting a journal note.
Takes precedence over `vulpea-journal--type' in the template function.")

(defun vulpea-journal--detect-buffer-type ()
  "Detect journal type from current buffer's filetags.
Returns `work', `personal', or nil if not a journal buffer."
  (when-let* ((file (buffer-file-name))
              (_ (string-match-p "/daily/" file)))
    (let ((tags (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward "^#\\+filetags:.*" nil t)
                    (match-string 0)))))
      (cond
       ((and tags (string-match-p "work-notes" tags)) 'work)
       ((and tags (string-match-p "personal-notes" tags)) 'personal)))))

(defun vulpea-journal-template+ (_date)
  "Dynamic template function for `vulpea-journal-default-template'.
Dispatches to work or personal config based on the active journal type."
  (pcase (or vulpea-journal--buffer-type vulpea-journal--type)
    ('work
     (list :file-name "daily/%Y-%m-work-notes.org"
           :title "%B %Y work notes"
           :tags '("work-notes")
           :entry-level 1
           :entry-title "%Y-%m-%d %A"
           :head "#+startup: overview"))
    ('personal
     (list :file-name "daily/%Y-%m-journal.org"
           :title "%B %Y personal notes"
           :tags '("personal-notes")
           :entry-level 1
           :entry-title "%Y-%m-%d %A"
           :head "#+startup: overview"))))

;;;###autoload
(defun vulpea-journal+ (type &optional date)
  "Open journal of TYPE for DATE.
TYPE is `work' or `personal'.  When DATE is nil, opens today."
  (let ((vulpea-journal--type type))
    (vulpea-journal (or date (current-time)))))
