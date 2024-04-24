;;; custom/general/autoload/expreg.el -*- lexical-binding: t; -*-

;;;###autoload
(defun expreg--line ()
  "Return a list of regions containing surrounding sentences."
  (ignore-errors
    (let (beg end)
      (end-of-visual-line)
      (setq end (point))
      (beginning-of-visual-line)
      (setq beg (point))
      `((line . ,(cons beg end))))))

;;;###autoload
(transient-define-prefix expreg-transient ()
  "expand/contract"
  [[("v" "expand" expreg-expand :transient t)]
   [("V" "contract" expreg-contract :transient t)]])
