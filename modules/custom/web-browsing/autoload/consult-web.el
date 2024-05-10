;;; custom/web-browsing/autoload/consult-web.el -*- lexical-binding: t; -*-

;;;###autoload
(defun consult-web--set-api-keys ()
  (cl-labels ((split-col (x) (split-string x ":")))
    (let ((keys-list
           `((consult-web-brave-api-key "api.search.brave.com")
             (consult-web-scopus-api-key "api.elsevier.com")
             (consult-web-google-customsearch-cx "www.googleapis.com" ,(-compose #'car #'split-col))
             (consult-web-google-customsearch-key "www.googleapis.com" ,(-compose #'cadr #'split-col)))))
      (dolist (k keys-list)
        (let ((key (car k))
              (host (nth 1 k))
              (fn (nth 2 k)))
          (set (intern (symbol-name key))
               (lambda ()
                 (when-let ((ps (auth-source-pick-first-password :host host)))
                   (if fn
                       (funcall fn ps)
                     ps)))))))))
