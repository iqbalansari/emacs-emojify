;; -*- lexical-binding: t; -*-
;; Another plugin to waste time in Emacs :sweat: :worried: :unamused: :p :) :p
;;
;; TODO: Custom images
;;       Benchmark
;;       Cleanup

(require 'json)
(require 'subr-x)

(defvar emoji-emoji-json (expand-file-name "emoji.json" (if load-file-name (file-name-directory load-file-name) default-directory)))
(defvar emoji-image-dir (expand-file-name "images" (if load-file-name (file-name-directory load-file-name) default-directory)))
(defvar emoji-parsed (let ((json-array-type 'list)
                           (json-object-type 'hash-table))
                       (json-read-file emoji-emoji-json)))

(defvar emoji-regexps (let ((emojis (hash-table-keys emoji-parsed)))
                        (regexp-opt emojis)))

;; (Eventually) Can be one of image, unicode, ascii
(defvar emoji-substitution-style 'image)



(defgroup emojify nil
  "Customization options for mu4e-alert"
  :group 'display
  :prefix "emojify-")



;; Customizations to control the enabling of emojify-mode

(defcustom emojify-inhibit-major-modes
  '(doc-view-mode
    pdf-view-mode
    image-mode
    help-mode
    magit-popup-mode)
  "Major modes where emojify mode should not be enabled."
  :type 'list
  :group 'emojify)

(defcustom emojify-inhibit-in-buffer-functions
  '(minibufferp emojify-ephemeral-buffer-p emojify-inhibit-major-mode-p emojify-helm-buffer-p)
  "Functions used to determine emojify-mode should be enabled in a buffer.

These functions are called with one argument, the buffer where emojify-mode
is about to be enabled, emojify is not enabled if any of the functions return
a non-nil value."
  :type 'hook
  :group 'emojify)

(defun emojify-ephemeral-buffer-p (buffer)
  (string-match-p "^ " (buffer-name buffer)))

(defun emojify-inhibit-major-mode-p (buffer)
  (memq (with-current-buffer buffer
          major-mode)
        emojify-inhibit-major-modes))

(defun emojify-helm-buffer-p (buffer)
  (string-match-p "\\*helm" (buffer-name buffer)))

(defun emojify-buffer-p (buffer)
  (not (run-hook-with-args-until-success 'emojify-inhibit-in-buffer-functions buffer)))



;; Customizations to control display of emojis

(defcustom emojify-prog-contexts
  'both
  "Contexts where emojis can be displayed in programming modes.

Possible values are
`comment' - Display emojis only in comments
`string'  - Display emojis only in strings
`both'    - Display emojis in comments and strings
`none'    - Do not display emojis in programming modes")

(defcustom emojify-inhibit-hooks
  '(emojify-inhibit-in-org-tags)
  "Functions used to if emoji should displayed at current point.

These functions are called with no arguments, the point is at the point
where emoji text is found."
  :type 'hook
  :group 'emojify)

(defun emojify-inhibit-in-org-tags ()
  (memq 'org-tag (face-at-point nil t)))

(defun emojify-valid-prog-context-p (beg end)
  (unless (or (not emojify-prog-contexts)
          (eq emojify-prog-contexts 'none))
    (let ((syntax-beg (syntax-ppss beg))
          (syntax-end (syntax-ppss end))
          (pos (pcase emojify-prog-contexts
                 (`string 3)
                 (`comment 4)
                 (`both 8))))
      (and (nth pos syntax-beg)
           (nth pos syntax-end)))))

(defun emojify-valid-text-context-p (beg end)
  (and (or (not (char-before beg))
           ;; 32 space since ?  (? followed by a space) is not readable
           ;; 34 is "  since?" confuses font-lock
           (memq (char-syntax (char-before beg))
                 ;; space
                 '(32
                   ;; start/end of string
                   34
                   ;; whitespace syntax
                   ?-
                   ;; comment start
                   ?<
                   ;; comment end, this handles text at start of line immediately
                   ;; after comment line in a multiline comment
                   ?>)))
       ;; The text is at the end of the buffer
       (or (not (char-after end))
           (memq (char-syntax (char-after end))
                 ;; space
                 '(32
                   ;; start/end of string
                   34
                   ;; whitespace syntax
                   ?-
                   ;; comment end
                   ?>)))))



;; Core functions and macros

(defsubst emojify-get-image (name)
  (let ((emoji-one (gethash name emoji-parsed)))
    (when emoji-one
      (create-image (expand-file-name (concat (gethash "unicode" emoji-one) ".png")
                                      emoji-image-dir)
                    ;; Use imagemagick if available (allows resizing images
                    (when (fboundp 'imagemagick-types)
                      'imagemagick)
                    nil
                    :ascent 'center
                    ;; no-op if imagemagick is not available
                    :height (default-font-height)))))

(defmacro emojify-with-saved-buffer-state (&rest forms)
  "Execute FORMS saving point and mark, match-data and buffer modification state
also inhibit buffer change, point motion hooks.

Used by `emojify-display-emojis-in-region' and `emojify-undisplay-emojis-in-region'"
  (declare (debug t) (indent 0))
  `(let ((inhibit-point-motion-hooks t))
     (with-silent-modifications
       (save-match-data
         (save-excursion
           ,@forms)))))

(defun emojify-display-emojis-in-region (beg end)
  "Display emojis in region.
BEG and END are the beginning and end of the region respectively"
  (emojify-with-saved-buffer-state
    (goto-char beg)
    (while (search-forward-regexp emoji-regexps end t)
      (let ((match-beginning (match-beginning 0))
            (match-end (match-end 0))
            (match (match-string-no-properties 0)))

        ;; Display unconditionally in non-prog mode
        (when (and (or (not (derived-mode-p 'prog-mode))
                       ;; In prog mode enable respecting `emojify-prog-contexts'
                       (emojify-valid-prog-context-p match-beginning
                                                     match-end))
                   ;; The text is at the beginning of the buffer
                   (emojify-valid-text-context-p match-beginning
                                            match-end)
                   ;; Allow user to inhibit display
                   (not (run-hook-with-args-until-success 'emojify-inhibit-hooks)))

          ;; TODO: Remove double checks
          (when (gethash match emoji-parsed)
            (add-text-properties match-beginning match-end (list 'display (pcase emoji-substitution-style
                                                                            (`image (emojify-get-image match)))
                                                                 'emojified t
                                                                 'point-entered (lambda (x y)
                                                                                  (emojify-undisplay-emojis-in-region match-beginning match-end t))
                                                                 'point-left (lambda (x y)
                                                                               (when (or (< match-end y)
                                                                                         (< y match-beginning))
                                                                                 (emojify-display-emojis-in-region match-beginning match-end)))))))))))

(defun emojify-undisplay-emojis-in-region (beg end &optional point-entered-p)
  "Undisplay the emojis in region.
BEG and END are the beginning and end of the region respectively"
  (emojify-with-saved-buffer-state
    (while (< beg end)
      ;; Get the start of emojified region in the region, the region is marked
      ;; with text-property `emojified' whose value is `t'. The region is marked
      ;; so that we do not inadvertently remove display composition or other
      ;; properties inserted by other packages.  This might fails too if a
      ;; package adds any of these properties between an emojified text, but
      ;; that situation is hopefully very rare and better than blindly removing
      ;; all text properties
      (let* ((emoji-start (text-property-any beg end 'emojified t))
             ;; Get the end emojified text, if we could not find the start set
             ;; emoji-end to region `end', this merely to make looping easier.
             (emoji-end (or (and emoji-start
                                 (text-property-not-all emoji-start end 'emojified t))
                            ;; If the emojified text is at the end of the region
                            ;; assume that end is the emojified text.
                            end)))
        ;; Proceed only if we got start of emojified text
        (when emoji-start
          ;; Remove the properties
          (remove-text-properties emoji-start emoji-end (append (list 'emojified t
                                                                      'display t
                                                                      'composition t
                                                                      'point-entered t)
                                                                ;; Get this working
                                                                (unless point-entered-p
                                                                  '(point-left t)))))
        ;; Setup the next iteration
        (setq beg emoji-end)))))

(defun emojify-after-change-function (beginning end len)
  (let ((inhibit-read-only t)
        ;; Extend the region to match the beginning of line where change began
        (region-end (save-excursion
                      (goto-char end)
                      (line-end-position)))
        ;; Extend the region to match the end of line where change ended
        (region-start (save-excursion
                        (goto-char beginning)
                        (line-beginning-position))))
    ;; Remove previously added emojis
    (emojify-undisplay-emojis-in-region region-start region-end)
    ;; Add emojis to the region
    (emojify-display-emojis-in-region region-start region-end)))



;; Resize emojis on text resize
;; (defadvice text-scale-increase (after emojify-resize-emojis (&rest ignored))
;;   (font-lock-fontify-buffer))

;; (ad-activate 'text-scale-increase)

(defun emojify-turn-on-emojify-mode ()
  (when (emojify-buffer-p (current-buffer))
    (if font-lock-defaults
        (jit-lock-register #'emojify-display-emojis-in-region)
        (save-restriction
          (widen)
          (emojify-display-emojis-in-region (point-min) (point-max))))
    ;; Make sure emojis are displayed in newly inserted text
    (add-hook 'after-change-functions #'emojify-after-change-function t t)))

(defun emojify-turn-off-emojify-mode ()
  ;; Remove currently displayed emojis
  (save-restriction
    (widen)
    (emojify-undisplay-emojis-in-region (point-min) (point-max)))
  (jit-lock-unregister #'emojify-display-emojis-in-region)
  ;; Make sure emojis are displayed in newly inserted text
  (remove-hook 'after-change-functions #'emojify-after-change-function t))

(define-minor-mode emojify-mode
  "Emojify mode"
  :init-value nil
  (if emojify-mode
      ;; Turn on
      (emojify-turn-on-emojify-mode)
    ;; Turn off
    (emojify-turn-off-emojify-mode)))

(define-globalized-minor-mode global-emojify-mode
  emojify-mode emojify-turn-on-emojify-mode
  :init-value nil)
