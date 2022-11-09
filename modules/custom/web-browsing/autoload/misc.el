;;; custom/web-browsing/autoload/misc.el -*- lexical-binding: t; -*-

(defun browser-history-make-db-copy ()
  (let* ((hist-db
          (substitute-in-file-name
           "$HOME/Library/Application Support/BraveSoftware/Brave-Browser/Default/History"))
         (new-fname (concat (temporary-file-directory) "bhist")))
    (copy-file hist-db new-fname :overwite)
    new-fname))

(defun browser-history-search ()
  (interactive)
  (setq pick-url (lambda (link) (car (alist-get link rows nil nil #'string=))))
  (let* ((db (sqlite-open (browser-history-make-db-copy)))
         (_ (setq rows (sqlite-execute
                        db "select title, url from urls order by last_visit_time desc")))
         (completion-extra-properties
          '(:annotation-function
            (lambda (k)
              (let* ((url (string-trim-right
                           (replace-regexp-in-string
                            "http\\(s\\|\\)://"
                            "" (funcall pick-url k))
                           "/")))
                (format "\n\t%s" url)))))
         (keep-order (lambda (completions)
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             `(metadata (display-sort-function . ,#'identity))
                           (complete-with-action
                            action completions string pred)))))
         (col (funcall keep-order rows))
         (pred (lambda (x) (not (string-blank-p (car x)))))
         (selection (completing-read "Browser history: " col pred)))
    (when-let (url (funcall pick-url selection))
      (browse-url url))))
