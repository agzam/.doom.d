LOGFILE = /tmp/emacs-cron.log
EMACS = emacs
DOOM_DIR = $(HOME)/.emacs.d
EMACS_BATCH = $(EMACS) --batch --no-window-system
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	INSTALL_CMD = brew install
else ifeq ($(shell test -f /etc/arch-release && echo yes),yes)
    INSTALL_CMD = sudo pacman -S --noconfirm
endif

.PHONY: help pdf-tools-build vterm
help:
	@grep -E '^[a-zA-Z_-]+:' Makefile | grep -v '.PHONY' | sed 's/:.*//g' | sort

pdf-tools:
	@echo "[$(shell date -Iseconds)] Starting pdf-tools rebuild" | tee -a $(LOGFILE)
	@CMD=$$(DISPLAY="" $(EMACS_BATCH) --eval \
	"(progn \
		(let* ((default-directory \"$(DOOM_DIR)/.local/straight/repos\"))									\
			(normal-top-level-add-subdirs-to-load-path)														\
			(require 'straight)																				\
			(let* ((build-dir (expand-file-name																\
								(format \".local/straight/build-%s\" emacs-version)                         \
								user-emacs-directory))														\
					(cmd (format \"%s/pdf-tools/build/server/autobuild -i %s/pdf-tools/ \"                  \
							build-dir build-dir)))															\
				(princ cmd)))																				\
	)" 2>/dev/null) && \
	$$CMD 2>&1 | tee -a $(LOGFILE)
	@echo "[$(shell date -Iseconds)] Finished pdf-tools rebuild (exit: $$?)" | tee -a $(LOGFILE)

vterm:
	@echo "Checking for required dependencies..."
	@which cmake > /dev/null 2>&1 || (echo "Installing cmake..." && $(INSTALL_CMD) cmake)
	@which libtool > /dev/null 2>&1 || (echo "Installing libtool..." && $(INSTALL_CMD) libtool)

	@echo "\n[$(shell date -Iseconds)] Starting vterm rebuild" | tee -a $(LOGFILE)
	DISPLAY="" $(EMACS_BATCH) --eval \
	"(progn \
		(let ((default-directory \"$(DOOM_DIR)/.local/straight/repos\")) \
			(normal-top-level-add-subdirs-to-load-path)) \
		(require 'cl-macs) \
		(require 'straight) \
		(let ((default-directory (straight--build-dir))) \
			(normal-top-level-add-subdirs-to-load-path)) \
		(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)) \
				  ((symbol-function 'y-or-n-p) (lambda (&rest _) t))) \
			(load \"vterm\" t) \
			(let ((vterm-install-buffer-name \"*vterm-compile*\")) \
				(vterm-module-compile) \
				(with-current-buffer vterm-install-buffer-name \
					(princ (buffer-string))))))" 2>&1 | tee -a $(LOGFILE)
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
