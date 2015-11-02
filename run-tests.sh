#!/bin/sh

cask exec emacs -batch -l ert -l emojify-tests.el -f ert-run-tests-batch-and-exit "$@"
