EMACS ?= emacs
FILE = agent-shell-usage-limits.el

.PHONY: check compile checkdoc test clean

check: compile checkdoc test

compile:
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILE)

# In batch mode checkdoc reports through `display-warning', so count those.
checkdoc:
	$(EMACS) -Q --batch \
	  --eval '(defvar checkdoc-issues 0)' \
	  --eval '(advice-add (quote display-warning) :before (lambda (&rest _) (setq checkdoc-issues (1+ checkdoc-issues))))' \
	  --eval '(checkdoc-file "$(FILE)")' \
	  --eval '(if (> checkdoc-issues 0) (kill-emacs 1) (princ "checkdoc: no issues\n"))'

test:
	$(EMACS) -Q --batch -L . \
	  -l tests/agent-shell-usage-limits-test.el \
	  -f ert-run-tests-batch-and-exit

# Needs package-lint installed in ELPA_DIR, with MELPA's archive contents
# downloaded there (package-lint checks that agent-shell is installable).
ELPA_DIR ?= $(HOME)/.emacs.d/elpa
lint:
	$(EMACS) -Q --batch \
	  --eval '(setq package-user-dir "$(ELPA_DIR)")' \
	  --eval '(require (quote package))' \
	  --eval '(add-to-list (quote package-archives) (cons "melpa" "https://melpa.org/packages/"))' \
	  --eval '(package-initialize)' \
	  --eval '(package-read-all-archive-contents)' \
	  --eval '(require (quote package-lint))' \
	  -f package-lint-batch-and-exit $(FILE)

clean:
	rm -f *.elc tests/*.elc
