;;; custom/general/autoload/expand-region.el -*- lexical-binding: t; -*-

;;;###autoload
(defun er/mark-between (&optional inclusive?)
  "Mark between various delimeters within same line.
   With INCLUSIVE? marks with delimiters."
  (interactive)
  (require 'evil-common)
  (let* ((pairs '(("/" "/") ("=" "=") ("~" "~") ("(" ")") ("\\[" "\\]") ("<" ">") ("'" "'") ("\"" "\"") (" " " "))))
    (dolist (pair pairs)
      (let* ((prev (point))
             (reg (ignore-errors (evil-select-paren
                                  (car pair) (cadr pair)
                                  nil
                                  nil
                                  nil 1
                                  inclusive?))))
        (when (and reg
                   (<= (line-beginning-position) (car reg))
                   (<= (nth 1 reg) (line-end-position)))
          (deactivate-mark t)
          (goto-char (nth 1 reg))
          (set-mark (point))
          (goto-char (car reg))
          (cl-return reg))))))

;;;###autoload
(defun er/mark-eovl ()
  "Marks visual line to the end."
  (interactive)
  (if (use-region-p)
      (set-mark (region-beginning))
    (set-mark (point)))
  (end-of-visual-line)
  (exchange-point-and-mark))

;;;###autoload
(defun er/mark-bovl ()
  "Marks visual line to the beginning."
  (interactive)
  (if (use-region-p)
      (set-mark (region-end))
    (set-mark (point)))
  (beginning-of-visual-line))


;;;###autoload
(defun er/mark-eol ()
  "Marks entire logical line to the end."
  (interactive)
  (if (use-region-p)
      (set-mark (region-beginning))
    (set-mark (point)))
  (evil-end-of-line))

;;;###autoload
(defun er/mark-bol ()
  "Marks entire logical line to the beginning."
  (interactive)
  (if (use-region-p)
      (set-mark (region-end))
    (set-mark (point)))
  (evil-beginning-of-line))
