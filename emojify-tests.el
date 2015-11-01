;;; Code:

(unless noninteractive
  (let ((default-directory (expand-file-name ".cask/")))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path default-directory)

(require 'emojify)
(require 'ert)
(require 'el-mock)
(require 'cl)

(defmacro emojify-tests-with-saved-custumizations (&rest forms)
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
         (with-current-buffer test-buffer
           ;; Rename it uniquely so that subsequent buffers do not conflict with it
           (rename-uniquely)
           ;; Save all possible customizations
           (emojify-tests-with-saved-custumizations
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
  (emojify-tests-with-emojified-static-buffer ":)"
    (emojify-tests-should-be-emojified (point-min))
    (should (equal (get-text-property (point-min) 'emojify-buffer) (current-buffer)))
    (should (equal (get-text-property (point-min) 'emojify-start) (point-min)))
    (should (equal (get-text-property (point-min) 'emojify-end) (point-max)))
    (should (equal (get-text-property (point-min) 'emojify-text)  ":)"))))

(ert-deftest emojify-tests-simple-github-emoji-test ()
  (emojify-tests-with-emojified-static-buffer ":smile:"
    (emojify-tests-should-be-emojified (point-min))
    (should (equal (get-text-property (point) 'emojify-buffer) (current-buffer)))
    (should (equal (get-text-property (point-min) 'emojify-start) (point-min)))
    (should (equal (get-text-property (point) 'emojify-end) (point-max)))
    (should (equal (get-text-property (point) 'emojify-text)  ":smile:"))))

(ert-deftest emojify-tests-emoji-uncovering ()
  (emojify-tests-with-emojified-buffer " :)"
    (setq emojify-point-entered-behaviour 'uncover)
    (goto-char (1+ (point-min)))
    (emojify-tests-should-be-uncovered (point))))

(ert-deftest emojify-tests-emoji-echoing ()
  (emojify-tests-with-emojified-buffer " :)"
    (with-mock
      (mock (message ":)"))
      (setq emojify-point-entered-behaviour 'echo)
      (goto-char (1+ (point-min)))
      (emojify-tests-should-be-emojified (point)))))

(ert-deftest emojify-tests-custom-point-entered-function ()
  (emojify-tests-with-emojified-buffer " :)"
    (setq emojify-point-entered-behaviour (lambda (buffer emoji-text emoji-start emoji-end)
                                            (should (equal buffer (current-buffer)))
                                            (should (equal emoji-text ":)"))
                                            (should (equal emoji-start (1+ (point-min))))
                                            (should (equal emoji-start (point-max)))))
    (goto-char (1+ (point-min)))
    (emojify-tests-should-be-emojified (point))))

(ert-deftest emojify-tests-emojify-setting-styles ()
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
  (emojify-tests-with-emojified-static-buffer ";; :) :smile:\n\":smile:\""
    (let* ((comment-ascii-emoji-pos (+ 3 (point-min)))
           (comment-github-emoji-pos (+ comment-ascii-emoji-pos (length ":) ")))
           (string-github-emoji-pos (1+ (line-beginning-position 2))))
      (emacs-lisp-mode)
      (setq emojify-prog-contexts 'both)
      (emojify-redisplay-emojis)
      (emojify-tests-should-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-be-emojified string-github-emoji-pos)

      (setq emojify-prog-contexts 'comments)
      (emojify-redisplay-emojis)
      (emojify-tests-should-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-not-be-emojified string-github-emoji-pos)

      (setq emojify-prog-contexts 'string)
      (emojify-redisplay-emojis)
      (emojify-tests-should-not-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-not-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-be-emojified string-github-emoji-pos)

      (setq emojify-prog-contexts 'none)
      (emojify-redisplay-emojis)
      (emojify-tests-should-not-be-emojified comment-ascii-emoji-pos)
      (emojify-tests-should-not-be-emojified comment-github-emoji-pos)
      (emojify-tests-should-not-be-emojified string-github-emoji-pos))))

(provide 'emojify-tests)
;;; emojify-tests.el ends here
