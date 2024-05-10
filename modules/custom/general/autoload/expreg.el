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
   [("V" "contract" expreg-contract :transient t)]]
  [:hide always
         ("RET" "ignore" transient-quit-one)
         ("y" "y" evil-yank)
         ("d" "d" evil-delete)
         ("x" "x" evil-delete-char)
         ("p" "p" evil-paste-after)
         ("P" "P" evil-paste-before)
         ("r" "r" evil-replace)
         ("c" "c" evil-change)
         ("s" "s" evil-surround-region)
         ("R" "R" evil-multiedit-match-all)
         ("o" "o" exchange-point-and-mark :transient t)
         ("0" "0" evil-beginning-of-line :transient t)
         ("$" "$" evil-end-of-line :transient t)
         ("k" "k" evil-previous-visual-line :transient t)
         ("j" "j" evil-next-visual-line :transient t)
         ("h" "h" evil-backward-char :transient t)
         ("l" "l" evil-forward-char :transient t)
         ("%" "%" evilmi-jump-items :transient t)
         ("C-;" "embark-action" embark-act)
         (">" "indent" indent-rigidly)
         ("~" "invert" evil-invert-char)
         ("SPC" "space" (lambda () (interactive) (funcall (general-simulate-key "SPC"))))])
