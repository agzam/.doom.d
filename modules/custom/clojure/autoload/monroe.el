;;; custom/clojure/autoload/monroe.el -*- lexical-binding: t; -*-

(defvar spacehammer-monroe--reconnect-timer nil
  "Timer for auto-reconnecting to Spacehammer nREPL.")

;;;###autoload
(defun spacehammer-monroe--cleanup ()
  "Kill stale monroe processes and buffers."
  (dolist (p (process-list))
    (when (string-prefix-p "monroe" (process-name p))
      (delete-process p)))
  (dolist (b (buffer-list))
    (when (string-match-p "\\*monroe" (buffer-name b))
      (kill-buffer b))))

;;;###autoload
(defun spacehammer-monroe-connect ()
  "Connect to Spacehammer's nREPL server via Monroe."
  (interactive)
  (spacehammer-monroe--cleanup)
  (monroe (format "localhost:%d" monroe-default-port)))

;;;###autoload
(defun spacehammer-monroe--try-reconnect ()
  "Attempt to reconnect to Spacehammer nREPL."
  (when spacehammer-monroe--reconnect-timer
    (cancel-timer spacehammer-monroe--reconnect-timer)
    (setq spacehammer-monroe--reconnect-timer nil))
  (condition-case nil
      (progn
        (spacehammer-monroe--cleanup)
        (monroe (format "localhost:%d" monroe-default-port))
        (message "Spacehammer nREPL: reconnected"))
    (error
     (setq spacehammer-monroe--reconnect-timer
           (run-with-timer 1 nil #'spacehammer-monroe--try-reconnect)))))

;;;###autoload
(defun spacehammer-monroe--sentinel (orig-fn process message)
  "Advise monroe sentinel to auto-reconnect on disconnect."
  (funcall orig-fn process message)
  (when (and (string-prefix-p "monroe/" (process-name process))
             (not (process-live-p process)))
    (message "Spacehammer nREPL: connection lost, reconnecting...")
    (setq spacehammer-monroe--reconnect-timer
          (run-with-timer 1.5 nil #'spacehammer-monroe--try-reconnect))))
