(unless noninteractive
  (let ((default-directory (expand-file-name ".cask/")))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path default-directory)

(require 'emojify)
(require 'ert)
(require 'el-mock)
(require 'cl)

;; Used for testing integration with programming modes
(require 'org)
(require 'org-agenda)
(require 'cc-mode)

(defmacro emojify-tests-with-saved-customizations (&rest forms)
  "Run forms saving current customizations and restoring them on completion.

Helps isolate tests from each other's customizations."
  (declare (indent 0))
  `(let ((emojify-saved-emoji-json emojify-emoji-json)
         (emojify-saved-image-dir emojify-image-dir)
         (emojify-saved-substitution-style emojify-substitution-style)
         (emojify-saved-inhibit-major-modes emojify-inhibit-major-modes)
         (emojify-saved-inhibit-in-buffer-functions emojify-inhibit-in-buffer-functions)
         (emojify-saved-preferred-style emojify-preferred-style)
         (emojify-saved-prog-contexts emojify-prog-contexts)
         (emojify-saved-inhibit-functions emojify-inhibit-functions)
         (emojify-saved-point-entered-behaviour emojify-point-entered-behaviour)
         (emojify-saved-show-help emojify-show-help))
     (unwind-protect
         (progn
           ,@forms)
       (setq emojify-emoji-json emojify-saved-emoji-json
             emojify-image-dir emojify-saved-image-dir
             emojify-substitution-style emojify-saved-substitution-style
             emojify-inhibit-major-modes emojify-saved-inhibit-major-modes
             emojify-inhibit-in-buffer-functions emojify-saved-inhibit-in-buffer-functions
             emojify-prog-contexts emojify-saved-prog-contexts
             emojify-inhibit-functions emojify-saved-inhibit-functions
             emojify-point-entered-behaviour emojify-saved-point-entered-behaviour
             emojify-show-help emojify-saved-show-help)
       ;; This as a side-effect also re-reads JSON data so no need to
       ;; re-adjust changes to emojify-emoji-json
       (emojify-set-preferred-style emojify-saved-preferred-style))))

(defmacro emojify-tests-with-emojified-buffer (str &rest forms)
  (declare (indent 1))
  ;; Run tests in a new buffer
  `(let ((test-buffer (get-buffer-create " *emojify-test-buffer*")))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer test-buffer)
           ;; Rename it uniquely so that subsequent buffers do not conflict with it
           (rename-uniquely)
           ;; Save all possible customizations
           (emojify-tests-with-saved-customizations
             (setq emojify-point-entered-behaviour nil)
             (insert ,str)
             (emojify-mode +1)
             ;; Force refontification since JIT does it lazily
             (emojify-display-emojis-in-region (point-min) (point-max))
             (goto-char (point-min))
             ,@forms))
       ;; Keep the buffer around for interactive tests, helps debugging failing
       ;; tests
       (when noninteractive
         (kill-buffer test-buffer)))))

(defmacro emojify-tests-with-emojified-static-buffer (str &rest forms)
  (declare (indent 1))
  `(emojify-tests-with-emojified-buffer ,str
     (emojify-with-saved-buffer-state
       ,@forms)))

(defmacro emojify-tests-should-be-emojified (point)
  `(progn
     (should-not (get-text-property ,point 'point-left))
     (should (get-text-property ,point 'emojified))
     (should (get-text-property ,point 'emojify-buffer))
     (should (get-text-property ,point 'emojify-start))
     (should (get-text-property ,point 'emojify-end))
     (should (get-text-property ,point 'emojify-text))
     (should (get-text-property ,point 'display))
     (should (get-text-property ,point 'point-entered))))

(defmacro emojify-tests-should-not-be-emojified (point)
  `(progn
     (should-not (get-text-property ,point 'point-left))
     (should-not (get-text-property ,point 'emojified))
     (should-not (get-text-property ,point 'emojify-buffer))
     (should-not (get-text-property ,point 'emojify-start))
     (should-not (get-text-property ,point 'emojify-end))
     (should-not (get-text-property ,point 'emojify-text))
     (should-not (get-text-property ,point 'display))
     (should-not (get-text-property ,point 'point-entered))))

(defmacro emojify-tests-should-be-uncovered (point)
  `(progn
     (should (get-text-property ,point 'point-left))
     (should (get-text-property ,point 'emojified))
     (should-not (get-text-property ,point 'emojify-buffer))
     (should-not (get-text-property ,point 'emojify-start))
     (should-not (get-text-property ,point 'emojify-end))
     (should-not (get-text-property ,point 'emojify-text))
     (should-not (get-text-property ,point 'display))
     (should-not (get-text-property ,point 'point-entered))))

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
  (emojify-tests-with-emojified-static-buffer ":) :smile:"
    (let ((ascii-emoji-pos (point-min))
          (github-emoji-pos (+ (point-min) (length ":) "))))
      (emojify-set-preferred-style 'github)

      ;; Ascii emoji should not be displayed
      (emojify-tests-should-not-be-emojified ascii-emoji-pos)

      ;; Github emojis should be displayed
      (emojify-tests-should-be-emojified github-emoji-pos)

      (emojify-set-preferred-style 'ascii)

      ;; Ascii emoji should be displayed
      (emojify-tests-should-be-emojified ascii-emoji-pos)

      ;; Github emojis should not be displayed
      (emojify-tests-should-not-be-emojified github-emoji-pos)

      (emojify-set-preferred-style 'all)

      ;; Ascii emoji should not be displayed
      (emojify-tests-should-be-emojified ascii-emoji-pos)

      ;; Github emojis should be displayed
      (emojify-tests-should-be-emojified github-emoji-pos))))

(ert-deftest emojify-tests-prog-contexts ()
  :tags '(prog contextual)
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

(ert-deftest emojify-tests-text-contexts ()
  :tags '(text contextual)
  ;; At start of comment
  (emojify-tests-with-emojified-static-buffer ";:smile:"
    (emacs-lisp-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-be-emojified (1+ (point-min))))

  ;; In comment after space
  (emojify-tests-with-emojified-static-buffer "; :smile:"
    (emacs-lisp-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-be-emojified (+ 2 (point-min))))

  ;; Inside a comment
  (emojify-tests-with-emojified-static-buffer "/**\n:)"
    (c-mode)
    (emojify-redisplay-emojis)
    (emojify-tests-should-be-emojified (line-beginning-position 2)))

  ;; Immediately after a bracket
  (emojify-tests-with-emojified-static-buffer "(:smile:"
    (emojify-tests-should-not-be-emojified (1+ (point-min))))

  ;; Immediately after a word
  (emojify-tests-with-emojified-static-buffer "A:)"
    (emojify-tests-should-not-be-emojified (1+ (point-min))))

  ;; Immediately before a word
  (emojify-tests-with-emojified-static-buffer ":)A"
    (emojify-tests-should-not-be-emojified (1+ (point-min))))

  ;; Immediately before a closing bracket
  (emojify-tests-with-emojified-static-buffer ":))"
    (emojify-tests-should-be-emojified (1+ (point-min))))

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

(ert-deftest emojify-tests-emojifying-lists ()
  :tags '(contextual)
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
    (org-mode)
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
        (isearch-printing-char ?b)
        (isearch-printing-char ?o)

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

(provide 'emojify-tests)
;;; emojify-tests.el ends here
