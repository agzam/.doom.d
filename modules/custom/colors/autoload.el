;;; custom/colors/autoloads.el -*- lexical-binding: t; -*-

(defun colors/get-current-period ()
  (let ((cur-hr (nth 2 (decode-time))))
    (caddr (cl-loop for (period . hour) in colors--time-periods
                    when (<= hour cur-hr)
                    collect period))))

(defun colors/load-theme ()
  "Loads theme based on time of day using `colors--themes'"
  (interactive)
  (let ((theme (alist-get (colors/get-current-period) colors--themes)))
    (funcall 'load-theme theme :no-ask)))

(defvar colors/next-theme-iterator nil)

(iter-yield-from colors/next-theme-iterator)

;;;###autoload (autoload 'colors/cycle-theme "custom/colors/autoload" nil t)
;; (defun colors/cycle-theme (&optional prev)
;;   (interactive)
;;   (when (null colors/next-theme-iterator)
;;     (let* ((nxt (iter-lambda (themes)
;;                   (let ((tmp-lst themes))
;;                     (while tmp-lst
;;                       (iter-yield (pop tmp-lst))
;;                       (when (null tmp-lst)
;;                         (setq tmp-lst themes)))))))
;;       (setq colors/next-theme-iterator
;;             (funcall nxt (seq-map 'cdr colors--themes)))))
;;   (if prev

;;       )
;;   (load-theme (iter-next colors/next-theme-iterator)))

;; (defhydra +hydra/cycle-theme (:hint nil :color red)
;;   "Change theme: _n_:next, _p_:previous, _c_:
;; "
;;   ("n" )
;;   )
