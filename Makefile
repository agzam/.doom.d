LOGFILE = /tmp/emacs-cron.log
EMACS = emacs
DOOM_DIR = $(HOME)/.emacs.d
EMACS_BATCH = $(EMACS) --batch --no-window-system

.PHONY: help pdf-tools-build vterm
help:
	@grep -E '^[a-zA-Z_-]+:' Makefile | grep -v '.PHONY' | sed 's/:.*//g' | sort

pdf-tools:
	@echo "[$(shell date -Iseconds)] Starting pdf-tools rebuild" | tee -a $(LOGFILE)
	DISPLAY="" $(EMACS_BATCH) --eval \
	"(progn \
		(let ((default-directory \"$(DOOM_DIR)/.local/straight/repos\")) \
			(normal-top-level-add-subdirs-to-load-path)) \
		(require 'pdf-tools) \
		(pdf-tools-install t) \
	)" 2>&1 | tee -a $(LOGFILE)
	@echo "[$(shell date -Iseconds)] Finished pdf-tools rebuild (exit: $$?)" | tee -a $(LOGFILE)

vterm:
	@echo "\n[$(shell date -Iseconds)] Starting vterm rebuild" | tee -a $(LOGFILE)
	DISPLAY="" $(EMACS_BATCH) --eval \
	"(progn \
		(let ((default-directory \"$(DOOM_DIR)/.local/straight/repos\")) \
			(normal-top-level-add-subdirs-to-load-path)) \
		(require 'cl-macs) \
		(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)) \
					((symbol-function 'y-or-n-p) (lambda (&rest _) t))) \
			(require 'vterm) \
			(vterm-module-compile)) \
		(with-current-buffer (get-buffer vterm-install-buffer-name) \
			(princ (buffer-string))) \
	)" 2>&1 | tee -a $(LOGFILE)
	@echo "[$(shell date -Iseconds)] Finished vterm rebuild (exit: $$?)\n" | tee -a $(LOGFILE)

org-roam-db-sync:
	@echo "\n[$(shell date -Iseconds)] Starting org-road-db-sync" | tee -a $(LOGFILE)
	DISPLAY="" $(EMACS_BATCH) --eval \
	"(progn \
		(let ((default-directory \"$(DOOM_DIR)/.local/straight/repos\")) \
			(normal-top-level-add-subdirs-to-load-path)) \
		(require 'org) \
		(require 'org-roam) \
		(require 'git-auto-commit-mode) \
		(setq org-roam-directory (expand-file-name \"~/Sync/org/\")) \
		(setq org-roam-db-location (concat \"$(DOOM_DIR)\" \"/.local/org-roam.db\")) \
		(org-roam-db-sync t) \
	)" 2>&1 | tee -a $(LOGFILE)
	@echo "[$(shell date -Iseconds)] Finished org-roam-db-sync (exit: $$?)\n" | tee -a $(LOGFILE)
