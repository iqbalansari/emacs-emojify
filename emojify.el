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

(defun emojify--emojify-buffer-p ()
  (not (or (minibufferp)
           (string-match-p "\\*helm" (buffer-name))
           (memq major-mode '(doc-view-mode pdf-view-mode image-mode help-mode)))))

;; (Eventually) Can be one of image, unicode, ascii
(defvar emoji-substitution-style 'image)



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
        ;; TODO Generalize these into a set of predicates
        (when (and (or (not (derived-mode-p 'prog-mode))
                       ;; TODO: How (in)efficient is this
                       (and (save-excursion
                              (goto-char match-beginning)
                              (nth 8 (syntax-ppss)))
                            (save-excursion
                              (goto-char match-end)
                              (nth 8 (syntax-ppss)))))
                   (or (not (char-before match-beginning))
                       ;; 32 space since ?  i.e. (? followed by a space is not readable)
                       ;; 34 is "  since?" confuses font-lock
                       ;; ?> Think multiline comments
                       (memq (char-syntax (char-before match-beginning)) '(32 34 ?- ?< ?>)))
                   (or (not (char-after match-end))
                       ;; 32 space since ?  i.e. (? followed by a space is not readable)
                       ;; 34 is "  since?" confuses font-lock
                       (memq (char-syntax (char-after match-end)) '(32  ?- ?> 34)))
                   (or (not (equal major-mode 'org-mode))
                       (equal (face-at-point) 'org-tag)))
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
  (when (emojify--emojify-buffer-p)
    (if font-lock-defaults
        (jit-lock-register #'emojify-display-emojis-in-region)
        (save-restriction
          (widen)
          (emojify-display-emojis-in-region (point-min) (point-max))))
    ;; Make sure emojis are displayed in newly inserted text
    (add-hook 'after-change-functions #'emojify-after-change-function t t)))

(defun emojify-turn-off-emojify-mode ()
  (when (emojify--emojify-buffer-p)
    ;; Remove currently displayed emojis
    (save-restriction
      (widen)
      (emojify-undisplay-emojis-in-region (point-min) (point-max)))
    (jit-lock-unregister #'emojify-display-emojis-in-region)
    ;; Make sure emojis are displayed in newly inserted text
    (remove-hook 'after-change-functions #'emojify-after-change-function t)))

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
