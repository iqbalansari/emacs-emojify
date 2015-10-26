;; -*- lexical-binding: t; -*-
;; Another plugin to waste time in Emacs :sweat: :worried: :unamused: :p :) :P
;;
;; TODO: Bug in org-capture (what causes this)
;;       Sometimes point is changed after adding emoji
;;       Custom images
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
           (string-match-p "helm" (symbol-name major-mode)))))

;; Can be one of image, unicode, ascii
(defvar emoji-substitution-style 'image)

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

(defun emojify--setup-emoji-display (start end match)
  "Emojify the text between start and end"
  (when (and (or (not (derived-mode-p 'prog-mode))
                 (nth 8 (syntax-ppss)))
             (not (and (char-before start)
                       (eq (char-syntax (char-before start)) ?w)))
             (not (and (char-after end)
                       (eq (char-syntax (char-after end)) ?w)))
             (or (not (equal major-mode 'org-mode))
                 (equal (face-at-point) 'org-tag)))
    (when (gethash match emoji-parsed)
      (add-text-properties start end (list 'display (pcase emoji-substitution-style
                                                      (`image (emojify-get-image match)))
                                           'emojified t
                                           'point-entered (lambda (x y) (message (format "%s" match))))))))

(defun emojify--emojify-region (beg end)
  (let ((inhibit-point-motion-hooks t))
    (with-silent-modifications
      (save-match-data
        (save-excursion
          (goto-char beg)
          (while (search-forward-regexp emoji-regexps end t)
            (emojify--setup-emoji-display (match-beginning 0)
                                          (match-end 0)
                                          (match-string 0))))))))

(defun emojify--unemojify-region (beg end &optional point-entered-p)
  (let ((inhibit-point-motion-hooks t))
    (with-silent-modifications
      (save-match-data
        (save-excursion
          (while (< beg end)
            (let* ((emoji-start (text-property-any beg end 'emojified t))
                   (emoji-end (or (and emoji-start
                                       (text-property-not-all emoji-start end 'emojified t))
                                  ;; If the emojified text is at the end of the region
                                  ;; assume that end is the emojified text.
                                  end)))
              ;; Proceed only if we got some text with emoji property
              (when emoji-start
                (remove-text-properties emoji-start emoji-end (append (list 'emojified t
                                                                            'display t
                                                                            'composition t
                                                                            'point-entered t)
                                                                      (unless point-entered-p
                                                                        '(point-left t)))))
              (setq beg emoji-end))))))))

(defun emojify--after-change-function (beginning end len)
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
    (emojify--unemojify-region region-start region-end)
    ;; Add emojis to the region
    (emojify--emojify-region region-start region-end)))

;; Resize emojis on text resize
;; (defadvice text-scale-increase (after emojify-resize-emojis (&rest ignored))
;;   (font-lock-fontify-buffer))

;; (ad-activate 'text-scale-increase)
(defun emojify-turn-on-emojify-mode ()
  (when (emojify--emojify-buffer-p)
    (save-restriction
      (widen)
      (emojify--emojify-region (point-min) (point-max)))
    ;; Make sure emojis are displayed in newly inserted text
    (add-hook 'after-change-functions #'emojify--after-change-function t t)))

(defun emojify-turn-off-emojify-mode ()
  (when (emojify--emojify-buffer-p)
    ;; Remove currently displayed emojis
    (save-restriction
      (widen)
      (emojify--unemojify-region (point-min) (point-max)))
    ;; Make sure emojis are displayed in newly inserted text
    (remove-hook 'after-change-functions #'emojify--after-change-function t)))

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
