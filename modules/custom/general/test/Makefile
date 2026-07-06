EMACS ?= emacs

.PHONY: test e2e

test:
	$(EMACS) -Q --batch -l ert -l windows-test.el -f ert-run-tests-batch-and-exit

e2e:
	./run-e2e.sh
