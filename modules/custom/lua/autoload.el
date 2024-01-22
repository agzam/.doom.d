;;; custom/lua/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun antifennel-install (arg)
  "Clone & install antifennel executable.
With a prefix, re-installs it."
  (interactive "P")
  (let ((repo-url "https://git.sr.ht/~technomancy/antifennel")
        (dir "/tmp/antifennel")
        (buf-name "install antifennel"))
    (when (and (executable-find "antifennel")
               (not arg))
      (user-error "antifennel is already installed"))
    (start-process-shell-command
     "Clone and install antifennel"
     buf-name
     (format (concat "rm -rf %s &&"
                     "git clone %s %s &&"
                     "cd %s && make && make install PREFIX=$HOME/.local &&"
                     ;; "rm -rf %s"
                     )
             dir repo-url dir dir dir))
    (pop-to-buffer buf-name)))

;;;###autoload
(defun fennel-fnlfmt-install (arg)
  "Clone & install fnlfmt executable.
With a prefix, re-installs it."
  (interactive "P")
  (let ((repo-url "https://git.sr.ht/~technomancy/fnlfmt")
        (dir "/tmp/fnlfmt")
        (buf-name "install fnlfmt"))
    (when (and (executable-find "fnlfmt")
               (not arg))
      (user-error "fnlfmt is already installed"))
    (start-process-shell-command
     "Clone and install fnlfmt"
     buf-name
     (format (concat "rm -rf %s &&"
                     "git clone %s %s &&"
                     "cd %s && make && make install PREFIX=$HOME/.local &&"
                     "rm -rf %s")
             dir repo-url dir dir dir))
    (pop-to-buffer buf-name)))


;;;###autoload
(defun fennel-lsp-server-install (arg)
  "Clone & install fennel-ls executable.
With a prefix, re-installs it."
  (interactive "P")
  (let ((repo-url "https://git.sr.ht/~xerool/fennel-ls")
        (dir "/tmp/fennel-ls")
        (buf-name "install fennel-ls"))
    (when (and (executable-find "fennel-ls")
               (not arg))
      (user-error "fennel-ls is already installed"))
    (start-process-shell-command
     "Clone and install fennel-ls"
     buf-name
     (format (concat "rm -rf %s &&"
                     "git clone %s %s &&"
                     "cd %s && make && make install PREFIX=$HOME/.local &&"
                     "rm -rf %s")
             dir repo-url dir dir dir))
    (pop-to-buffer buf-name)))
