;;; custom/org/autoload/verb.el -*- lexical-binding: t; -*-

;;;###autoload
(defun verb-post-response-h ()
  "Transform verb response to edn."
  (when verb-edn-response-enabled
    (with-current-buffer (current-buffer)
      (when (eq major-mode 'json-mode)
        (goto-char (point-min))
        (clojure-edn-json-transform)
        (clojure-mode)
        (verb-response-body-mode +1)
        (deactivate-mark)))))

;;;###autoload
(defun verb--request-spec-post-process-a (fn rs)
  "Turn edn to json before sending."
  (when verb-edn-request-enabled
    (when-let* ((body (slot-value rs :body))
                (headers (slot-value rs :headers))
                (json (unless (or (string-match-p "\\w\" ?:" body)
                                  ;; if the header explicitly set to accept edn
                                  (string-equal-ignore-case
                                   (or (alist-get "Accept" headers nil nil 'string-equal-ignore-case) "")
                                   "application/edn"))
                        (edn-string->json body))))
      (setf (slot-value rs :body) json)))
  (funcall fn rs))
