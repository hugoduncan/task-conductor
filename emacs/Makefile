.PHONY: test test-quiet test-one compile lint lint-package lint-all check help install-deps clean

EMACS ?= emacs
PKG_DIR := .

# Source files in dependency order for compilation
SRC_FILES := \
	$(PKG_DIR)/task-conductor-dev-env.el

# Test files
TEST_FILES := \
	$(PKG_DIR)/test/test-task-conductor-dev-env.el

# Stub files for testing (mocked dependencies)
STUB_FILES := \
	$(PKG_DIR)/test/stubs/claude-code.el

help:
	@echo "Targets:"
	@echo "  make install-deps - Install package dependencies"
	@echo "  make test     - Run ERT tests headless"
	@echo "  make test-quiet - Run ERT tests with minimal output (only failures)"
	@echo "  make test-one TEST=<selector> - Run specific test(s) by name or pattern"
	@echo "  make compile  - Byte-compile package sources"
	@echo "  make lint     - Run checkdoc on sources"
	@echo "  make lint-package - Run package-lint if available"
	@echo "  make lint-all - Run both lint and package-lint"
	@echo "  make check    - Compile then test"
	@echo "  make clean    - Remove compiled files"

install-deps:
	$(EMACS) -Q --batch \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
	  --eval "(package-initialize)" \
	  --eval "(package-refresh-contents)" \
	  --eval "(package-install 'cider)"

test: install-deps
	$(EMACS) -Q --batch -L $(PKG_DIR) -L $(PKG_DIR)/test -L $(PKG_DIR)/test/stubs \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
	  --eval "(package-initialize)" \
	  --eval "(setq ert-batch-backtrace-right-margin 80)" \
	  --eval "(setq ert-batch-print-level 3)" \
	  --eval "(setq ert-batch-print-length 10)" \
	  $(patsubst %,-l %,$(STUB_FILES)) \
	  $(patsubst %,-l %,$(SRC_FILES)) \
	  $(patsubst %,-l %,$(TEST_FILES)) \
	  -f ert-run-tests-batch-and-exit

test-quiet: install-deps
	$(EMACS) -Q --batch -L $(PKG_DIR) -L $(PKG_DIR)/test -L $(PKG_DIR)/test/stubs \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
	  --eval "(package-initialize)" \
	  --eval "(setq ert-quiet t)" \
	  --eval "(setq ert-batch-backtrace-right-margin 120)" \
	  --eval "(setq ert-batch-print-level 5)" \
	  --eval "(setq ert-batch-print-length 20)" \
	  $(patsubst %,-l %,$(STUB_FILES)) \
	  $(patsubst %,-l %,$(SRC_FILES)) \
	  $(patsubst %,-l %,$(TEST_FILES)) \
	  -f ert-run-tests-batch-and-exit

test-one: install-deps
ifndef TEST
	$(error TEST is not set. Usage: make test-one TEST=test-name-or-pattern)
endif
	$(EMACS) -Q --batch -L $(PKG_DIR) -L $(PKG_DIR)/test -L $(PKG_DIR)/test/stubs \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
	  --eval "(package-initialize)" \
	  --eval "(setq ert-batch-backtrace-right-margin 120)" \
	  --eval "(setq ert-batch-print-level 5)" \
	  --eval "(setq ert-batch-print-length 20)" \
	  $(patsubst %,-l %,$(STUB_FILES)) \
	  $(patsubst %,-l %,$(SRC_FILES)) \
	  $(patsubst %,-l %,$(TEST_FILES)) \
	  --eval "(ert-run-tests-batch-and-exit '$(TEST))"

compile: install-deps
	$(EMACS) -Q --batch -L $(PKG_DIR) -L $(PKG_DIR)/test/stubs \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
	  --eval "(package-initialize)" \
	  -f batch-byte-compile $(SRC_FILES)

lint:
	$(EMACS) -Q --batch -L $(PKG_DIR) -L $(PKG_DIR)/test/stubs \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
	  --eval "(package-initialize)" \
	  --eval "(require 'checkdoc)" \
	  --eval "(setq sentence-end-double-space nil)" \
	  --eval "(dolist (f '$(SRC_FILES)) (checkdoc-file f))"

lint-package:
	$(EMACS) -Q --batch -L $(PKG_DIR) -L $(PKG_DIR)/test/stubs \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))" \
	  --eval "(package-initialize)" \
	  --eval "(condition-case nil (progn (require 'package-lint) (setq package-lint-batch-fail-on-warn t command-line-args-left '(\"$(PKG_DIR)/task-conductor-dev-env.el\")) (package-lint-batch-and-exit)) (error (princ \"package-lint not available\\n\") (kill-emacs 0)))"

lint-all: lint lint-package

check: compile test

clean:
	rm -f $(PKG_DIR)/*.elc
