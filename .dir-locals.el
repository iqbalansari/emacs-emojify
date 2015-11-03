((emacs-lisp-mode
  (compile-command . "cask exec emacs -batch -l ert -l emojify-tests.el -f ert-run-tests-batch-and-exit")
  (eval ignore-errors
        (push (quote ("Tests" "(\\(\\<ert-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?" 2)) imenu-generic-expression)
        (when (string-match-p "test" (buffer-file-name))
          (setq emojify-inhibit-emojify-in-current-buffer-p t)))))
