;;; emojify-test.el --- Tests for emojify              -*- lexical-binding: t; -*-

;;; Commentary:
;;; Tests for emojify, major use case is to run from the command-line
;;; but can use can be interactively as well. Do M-x `eval-buffer' RET

;;; Code:

;; For interactive testing
(unless noninteractive
  (require 'test-helper (expand-file-name "test-helper.el")))

;; Used for testing integration with programming modes
(require 'org)
(require 'org-agenda)
(require 'cc-mode)

(ert-deftest emojify-tests-simple-ascii-emoji-test ()
  :tags '(ascii simple)
  (emojify-tests-with-emojified-static-buffer ":)"
    (emojify-tests-should-be-emojified (point-min))
    (should (equal (get-text-property (point-min) 'emojify-buffer) (current-buffer)))
    (should (equal (get-text-property (point-min) 'emojify-start) (point-min)))
    (should (equal (get-text-property (point-min) 'emojify-end) (point-max)))
    (should (equal (get-text-property (point-min) 'emojify-text)  ":)"))))

(ert-deftest emojify-tests-simple-github-emoji-test ()
  :tags '(github simple)
  (emojify-tests-with-emojified-static-buffer ":smile:"
    (emojify-tests-should-be-emojified (point-min))
    (should (equal (get-text-property (point) 'emojify-buffer) (current-buffer)))
    (should (equal (get-text-property (point-min) 'emojify-start) (point-min)))
    (should (equal (get-text-property (point) 'emojify-end) (point-max)))
    (should (equal (get-text-property (point) 'emojify-text)  ":smile:"))))

(ert-deftest emojify-tests-emojifying-on-comment-uncomment ()
  :tags '(core after-change)
  (emojify-tests-with-emojified-buffer ":smile:\n:)"
    (emacs-lisp-mode)
    (emojify-redisplay-emojis)
    (emojify-mode +1)
    (emojify-tests-should-not-be-emojified (line-beginning-position))
    (emojify-tests-should-not-be-emojified (line-beginning-position 2))

    (comment-region (point-min) (point-max))
    (emojify-tests-should-be-emojified (+ 3 (line-beginning-position)))
    (emojify-tests-should-be-emojified (+ 3 (line-beginning-position 2)))

    (uncomment-region (point-min) (point-max))
    (emojify-tests-should-not-be-emojified (line-beginning-position))
    (emojify-tests-should-not-be-emojified (line-beginning-position 2))))

(ert-deftest emojify-tests-emojifying-on-typing ()
  :tags '(core after-change)
  (emojify-tests-with-emojified-buffer ""
    (emacs-lisp-mode)
    (emojify-redisplay-emojis)
    (emojify-mode +1)
    (emojify-insert-string "; :)")
    (emojify-tests-should-be-emojified 4)
    (newline)
    (emojify-insert-string "; :smile")
    (emojify-tests-should-not-be-emojified (+ 4 (line-beginning-position)))
    (emojify-insert-string ":")
    (emojify-tests-should-be-emojified (+ 4 (line-beginning-position)))))

(ert-deftest emojify-tests-emoji-uncovering ()
  :tags '(behaviour point-motion)
  (emojify-tests-with-emojified-buffer " :)"
    (setq emojify-point-entered-behaviour 'uncover)
    (goto-char (1+ (point-min)))
    (emojify-tests-should-be-uncovered (point))))

(ert-deftest emojify-tests-emoji-echoing ()
  :tags '(behaviour point-motion)
  (emojify-tests-with-emojified-buffer " :)"
    (with-mock
      (mock (message ":)"))
      (setq emojify-point-entered-behaviour 'echo)
      (goto-char (1+ (point-min)))
      (emojify-tests-should-be-emojified (point)))))

(ert-deftest emojify-tests-custom-point-entered-function ()
  :tags '(behaviour point-motion)
  (emojify-tests-with-emojified-buffer " :)"
    (setq emojify-point-entered-behaviour (lambda (buffer emoji-text emoji-start emoji-end)
                                            (should (equal buffer (current-buffer)))
                                            (should (equal emoji-text ":)"))
                                            (should (equal emoji-start (1+ (point-min))))
                                            (should (equal emoji-start (point-max)))))
    (goto-char (1+ (point-min)))
    (emojify-tests-should-be-emojified (point))))

(ert-deftest emojify-tests-emojify-setting-styles ()
  :tags '(styles github ascii)
  (emojify-tests-with-emojified-static-buffer ":) 😄 :smile:"
    (let ((ascii-emoji-pos (point-min))
          (unicode-emoji-pos (+ (point-min) (length ":) ")))
          (github-emoji-pos (+ (point-min) (length ":) 😄 "))))

      (emojify-set-emoji-styles '(ascii))
      (emojify-tests-should-be-emojified ascii-emoji-pos)
      (emojify-tests-should-not-be-emojified unicode-emoji-pos)
      (emojify-tests-should-not-be-emojified github-emoji-pos)

      (emojify-set-emoji-styles '(unicode))
      (emojify-tests-should-not-be-emojified ascii-emoji-pos)
      (emojify-tests-should-be-emojified unicode-emoji-pos)
      (emojify-tests-should-not-be-emojified github-emoji-pos)

      (emojify-set-emoji-styles '(github))
      (emojify-tests-should-not-be-emojified ascii-emoji-pos)
      (emojify-tests-should-not-be-emojified unicode-emoji-pos)
      (emojify-tests-should-be-emojified github-emoji-pos)

      (emojify-set-emoji-styles '(ascii unicode github))
      (emojify-tests-should-be-emojified ascii-emoji-pos)
      (emojify-tests-should-be-emojified unicode-emoji-pos)
      (emojify-tests-should-be-emojified github-emoji-pos)

      (emojify-set-emoji-styles nil)
      (emojify-tests-should-not-be-emojified ascii-emoji-pos)
      (emojify-tests-should-not-be-emojified unicode-emoji-pos)
      (emojify-tests-should-not-be-emojified github-emoji-pos))))

(ert-deftest emojify-tests-prog-contexts ()
  :tags '(core prog contextual)
  (emojify-tests-with-emojified-static-buffer ";; :) :smile:\n\":smile:\"\n8)"
    (let* ((comment-ascii-emoji-pos (+ 3 (point-min)))
           (comment-github-emoji-pos (+ comment-ascii-emoji-pos (length ":) ")))
           (string-github-emoji-pos (1+ (line-beginning-position 2)))
           (prog-ascii-emoji-pos (1+ (line-beginning-position 3))))
      (emacs-lisp-mode)
      (setq emojify-prog-contexts 'both)
      (emojify-redisplay-emojis)
      (emojify-tests-should-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-be-emojified string-github-emoji-pos)
      (emojify-tests-should-not-be-emojified prog-ascii-emoji-pos)

      (setq emojify-prog-contexts 'comments)
      (emojify-redisplay-emojis)
      (emojify-tests-should-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-not-be-emojified string-github-emoji-pos)
      (emojify-tests-should-not-be-emojified prog-ascii-emoji-pos)

      (setq emojify-prog-contexts 'string)
      (emojify-redisplay-emojis)
      (emojify-tests-should-not-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-not-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-be-emojified string-github-emoji-pos)
      (emojify-tests-should-not-be-emojified prog-ascii-emoji-pos)

      (setq emojify-prog-contexts 'none)
      (emojify-redisplay-emojis)
      (emojify-tests-should-not-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-not-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-not-be-emojified string-github-emoji-pos)
      (emojify-tests-should-not-be-emojified prog-ascii-emoji-pos))))

(ert-deftest emojify-tests-ascii-emoji-contexts ()
  :tags '(core text contextual)
  ;; At start of comment
  (emojify-tests-with-emojified-static-buffer ";:)"
    (emacs-lisp-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-be-emojified (1+ (point-min))))

  ;; In comment after space
  (emojify-tests-with-emojified-static-buffer "; :)"
    (emacs-lisp-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-be-emojified (+ 2 (point-min))))

  ;; Inside a comment
  (emojify-tests-with-emojified-static-buffer "/**\n:)"
    (c-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-be-emojified (line-beginning-position 2)))

  ;; Immediately after a word
  (emojify-tests-with-emojified-static-buffer "A:)"
    (emojify-tests-should-not-be-emojified (1+ (point-min))))

  ;; Immediately before a word
  (emojify-tests-with-emojified-static-buffer ":)A"
    (emojify-tests-should-not-be-emojified (1+ (point-min))))

  ;; Immediately before a closing bracket
  (emojify-tests-with-emojified-static-buffer ":))"
    (emojify-tests-should-be-emojified (1+ (point-min))))

  ;; Immediately after a punctuation character
  (emojify-tests-with-emojified-static-buffer "!:)"
    (emojify-tests-should-not-be-emojified (1+ (point-min))))

  ;; Following a punctuation and a space
  (emojify-tests-with-emojified-static-buffer "! :)"
    (emojify-tests-should-be-emojified (+ 2 (point-min))))

  ;; Outside a comment
  (emojify-tests-with-emojified-static-buffer "/**/:)"
    (c-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-not-be-emojified (+ 4 (point-min))))

  ;; Surrounded by comments
  (emojify-tests-with-emojified-static-buffer "/*:)*/"
    (c-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-not-be-emojified (+ 2 (point-min)))))

(ert-deftest emojify-tests-multiple-emojis-in-sequence ()
  "See Github issue #6"
  (emojify-tests-with-emojified-static-buffer ":100::smile:
:100:a:smile:
🎆😃
🎆a😃
:100:😃
:100:a😃"
    ;; Github emojis
    (emojify-tests-should-be-emojified (point-min))
    (emojify-tests-should-be-emojified (+ (point-min) 5))
    (emojify-tests-should-be-emojified (line-beginning-position 1))
    (emojify-tests-should-be-emojified (+ (line-beginning-position 1) 6))

    ;; Unicode emojis
    (emojify-tests-should-be-emojified (line-beginning-position 2))
    (emojify-tests-should-be-emojified (+ (line-beginning-position 2) 1))
    (emojify-tests-should-be-emojified (line-beginning-position 3))
    (emojify-tests-should-be-emojified (+ (line-beginning-position 3) 3))

    ;; Mixed emojis
    (emojify-tests-should-be-emojified (line-beginning-position 4))
    (emojify-tests-should-be-emojified (+ (line-beginning-position 4) 5))
    (emojify-tests-should-be-emojified (line-beginning-position 4))
    (emojify-tests-should-be-emojified (+ (line-beginning-position 4) 6))))

(ert-deftest emojify-tests-emojifying-lists ()
  :tags '(core contextual)
  (emojify-tests-with-emojified-static-buffer ":]"
    (emojify-tests-should-be-emojified (point-min)))

  (emojify-tests-with-emojified-static-buffer "[ :]"
    (emojify-tests-should-not-be-emojified (+ 3 (point-min))))

  (emojify-tests-with-emojified-static-buffer ";; 8)"
    (emojify-tests-should-be-emojified (+ 3 (point-min))))

  (emojify-tests-with-emojified-static-buffer ";; (lambda () 8)"
    (emacs-lisp-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-not-be-emojified (+ 14 (point-min)))))

(ert-deftest emojify-tests-emojifying-org-mode-buffers ()
  :tags '(org-mode contextual)
  (emojify-tests-with-emojified-static-buffer "* Test :books:\n:books:"
    (org-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-not-be-emojified (1- (line-end-position)))
    (emojify-tests-should-be-emojified (line-beginning-position 2)))

  (emojify-tests-with-emojified-static-buffer "8)"
    ;; org-mode in Emacs v24.3 failed in read only buffers
    ;; if first item was not a headline
    (with-mock (stub org-set-startup-visibility => nil)
               (org-mode))
    (emojify-redisplay-emojis)
    (emojify-tests-should-not-be-emojified (point-min)))

  (emojify-tests-with-emojified-static-buffer "* 8)"
    (org-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-be-emojified (1- (point-max))))

  (emojify-tests-with-emojified-static-buffer "#+BEGIN_SRC emacs-lisp\n:)\n#+END_SRC"
    (org-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-not-be-emojified (line-beginning-position 2)))

  ;; TODO: This does not work yet
  ;; (emojify-tests-with-emojified-static-buffer "8) 8)"
  ;;   (org-mode)
  ;;   (emojify-redisplay-emojis)
  ;;   (emojify-tests-should-be-emojified (1- (point-max)))
  ;;   (emojify-tests-should-not-be-emojified (1+ (point-min))))

  (emojify-tests-with-emojified-static-buffer "* Test :books:\n:books:"
    (org-agenda-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-not-be-emojified (1- (line-end-position)))
    (emojify-tests-should-be-emojified (line-beginning-position 2))))

(ert-deftest emojify-tests-uncover-on-isearch ()
  :tags '(isearch)
  (emojify-tests-with-emojified-buffer "Testing isearch\n:books:"
    (with-mock
      ;; We do not want to be bothered with isearch messages
      (stub message => nil)
      (emojify-tests-should-be-emojified (line-beginning-position 2))
      (isearch-mode +1)
      (execute-kbd-macro ":book")
      ;; Emoji should be uncovered when point enters it in isearch-mode
      (emojify-tests-should-be-uncovered (line-beginning-position))
      (isearch-exit)
      ;; Emoji should be restored on leaving the underlying text
      (goto-char (point-min))
      (emojify-tests-should-be-emojified (line-beginning-position 2)))))

(ert-deftest emojify-tests-uncover-on-isearch-multiple-matches ()
  :tags '(isearch)
  (emojify-tests-with-emojified-buffer "Testing isearch\n:book:\n:books:"
    (let ((first-emoji-pos (line-beginning-position 2))
          (second-emoji-pos (line-beginning-position 3)))
      (with-mock
        ;; We do not want to be bothered with isearch messages
        (stub message => nil)
        (emojify-tests-should-be-emojified first-emoji-pos)
        (emojify-tests-should-be-emojified second-emoji-pos)

        (isearch-mode +1)
        ;; isearch-printing-char in Emacs 24.3 did not accept
        ;; any arguments
        (let ((last-command-event ?b)) (isearch-printing-char))
        (let ((last-command-event ?o)) (isearch-printing-char))

        ;; TODO: For some reason first one actually repeats backwards when
        ;; called non-interactively As such 2 more repeats are needed first to
        ;; go back to first match and second to actually search forward
        (isearch-repeat 'forward)
        (isearch-repeat 'forward)
        (isearch-repeat 'forward)

        (emojify-tests-should-be-emojified first-emoji-pos)
        (emojify-tests-should-be-uncovered second-emoji-pos)
        (isearch-exit)
        ;; Emoji should be restored on leaving the underlying text
        (goto-char (point-min))
        (emojify-tests-should-be-emojified first-emoji-pos)
        (emojify-tests-should-be-emojified second-emoji-pos)))))

;; So that tests can be run simply by doing `eval-buffer'
(unless noninteractive
  (ert "^emojify-"))

(provide 'emojify-test)
;;; emojify-test.el ends here
