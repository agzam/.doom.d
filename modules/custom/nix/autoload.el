;;;###autoload
(defun +nix/open-repl ()
  "Open a nix repl."
  (interactive)
  (nix-repl-show)
  (current-buffer))
