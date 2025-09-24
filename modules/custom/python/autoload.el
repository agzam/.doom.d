;;; custom/python/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun python-insert-ipdb ()
  "Insert an ipdb breakpoint."
  (interactive)
  (python-indent-line)
  (insert "import ipdb; ipdb.set_trace()\n")
  (python-indent-line)
  ;; When you hit 'n' (next) in the debugger, it might skip over the
  ;; next real line of code or behave unexpectedly, especially if the
  ;; next line is a control flow statement (if, for, return, etc.)
  ;; So, we need some dummy statement here
  (insert "pass  # debugger\n")
  (python-indent-line))
