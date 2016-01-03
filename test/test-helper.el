;;; test-helper.el --- Tests for emojify              -*- lexical-binding: t; -*-

;;; Commentary:
;;; Helpers to write tests for emojify

;;; Code:

;; Setup load-path, some of this is redundant when tests are run from the
;; command line
(let ((project-dir (locate-dominating-file (or (buffer-file-name) load-file-name)
                                           ".cask")))
  (if (not project-dir)
      (user-error "Could not locate project root")
    (let ((default-directory (expand-file-name (concat ".cask/" emacs-version) project-dir)))
      (normal-top-level-add-subdirs-to-load-path))

    (add-to-list 'load-path project-dir)))

;; Libs required for tests
(require 'ert)
(require 'el-mock)
(require 'cl)
(require 'noflet)

;; Load emojify
(require 'emojify)

;; Helper macros for tests
(defmacro emojify-tests-with-saved-customizations (&rest forms)
  "Run forms saving current customizations and restoring them on completion.

Helps isolate tests from each other's customizations."
  (declare (indent 0))
  `(let ((emojify-saved-emoji-json emojify-emoji-json)
         (emojify-saved-image-dir emojify-image-dir)
         (emojify-saved-display-style emojify-display-style)
         (emojify-saved-inhibit-major-modes emojify-inhibit-major-modes)
         (emojify-saved-inhibit-in-buffer-functions emojify-inhibit-in-buffer-functions)
         (emojify-saved-emoji-style emojify-emoji-styles)
         (emojify-saved-prog-contexts emojify-prog-contexts)
         (emojify-saved-inhibit-functions emojify-inhibit-functions)
         (emojify-saved-point-entered-behaviour emojify-point-entered-behaviour)
         (emojify-saved-show-help emojify-show-help)
         (emojify-saved-debug-mode emojify-debug-mode)
         (emojify-debug-mode t))
     (unwind-protect
         (progn
           (unless (file-exists-p emojify-image-dir)
             (emojify-download-emoji emojify-emoji-set))
           ,@forms)
       (setq emojify-emoji-json emojify-saved-emoji-json
             emojify-image-dir emojify-saved-image-dir
             emojify-display-style emojify-saved-display-style
             emojify-inhibit-major-modes emojify-saved-inhibit-major-modes
             emojify-inhibit-in-buffer-functions emojify-saved-inhibit-in-buffer-functions
             emojify-prog-contexts emojify-saved-prog-contexts
             emojify-inhibit-functions emojify-saved-inhibit-functions
             emojify-point-entered-behaviour emojify-saved-point-entered-behaviour
             emojify-show-help emojify-saved-show-help
             emojify-debug-mode emojify-saved-debug-mode)
       ;; This as a side-effect also re-reads JSON data so no need to
       ;; re-adjust changes to emojify-emoji-json
       (emojify-set-emoji-styles emojify-saved-emoji-style))))

(defmacro emojify-tests-with-emojified-buffer (str &rest forms)
  (declare (indent 1))
  ;; Run tests in a new buffer
  `(let ((test-buffer (get-buffer-create " *emojify-test-buffer*")))
     (noflet ((emojify-buffer-p (buffer)
                                (or (string-match-p "^ \\*emojify-test-buffer\\*" (buffer-name buffer))
                                    (funcall this-fn buffer))))
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
           (kill-buffer test-buffer))))))

(defmacro emojify-tests-with-emojified-static-buffer (str &rest forms)
  (declare (indent 1))
  `(emojify-tests-with-emojified-buffer ,str
     (emojify-with-saved-buffer-state
       ,@forms)))

(defmacro emojify-tests-should-be-emojified (point)
  `(progn
     (should-not (get-text-property ,point 'point-left))
     (should (get-text-property ,point 'emojified))
     (should (get-text-property ,point 'emojify-display))
     (should (get-text-property ,point 'emojify-buffer))
     (should (get-text-property ,point 'emojify-beginning))
     (should (get-text-property ,point 'emojify-end))
     (should (get-text-property ,point 'emojify-text))
     (should (get-text-property ,point 'display))
     (should (get-text-property ,point 'point-entered))))

(defmacro emojify-tests-should-not-be-emojified (point)
  `(progn
     (should-not (get-text-property ,point 'point-left))
     (should-not (get-text-property ,point 'emojified))
     (should-not (get-text-property ,point 'emojify-display))
     (should-not (get-text-property ,point 'emojify-buffer))
     (should-not (get-text-property ,point 'emojify-beginning))
     (should-not (get-text-property ,point 'emojify-end))
     (should-not (get-text-property ,point 'emojify-text))
     (should-not (get-text-property ,point 'display))
     (should-not (get-text-property ,point 'point-entered))))

(defmacro emojify-tests-should-be-uncovered (point)
  `(progn
     (should (get-text-property ,point 'point-left))
     (should (get-text-property ,point 'emojified))
     (should (get-text-property ,point 'emojify-buffer))
     (should (get-text-property ,point 'emojify-beginning))
     (should (get-text-property ,point 'emojify-end))
     (should (get-text-property ,point 'emojify-text))
     (should-not (get-text-property ,point 'point-entered))
     (should-not (get-text-property ,point 'display))))

(defun emojify-insert-string (string)
  (mapc (lambda (character)
          (insert character))
        (string-to-vector string)))

(provide 'test-helper)
;;; test-helper.el ends here
