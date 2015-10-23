;; Another plugin to waste time in Emacs :sweat: :worried: :unamused: :rocket:
;;
;; TODO: Handle non-font-lock modes like git-commit-mode, helm-mode
(require 'json)
(require 'subr-x)

(defvar emoji-emoji-json (expand-file-name "emoji.json" (if load-file-name (file-name-directory load-file-name) default-directory)))
(defvar emoji-image-dir (expand-file-name "images" (if load-file-name (file-name-directory load-file-name) default-directory)))
(defvar emoji-parsed (let ((json-array-type 'list)
                           (json-object-type 'hash-table))
                       (json-read-file emoji-emoji-json)))

(defun emoji-setup-emoji-display ()
  "Compose a sequence of characters into an emoji.
Regexp match data 0 points to the chars."
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntax-ppss-at-point (syntax-ppss))
         (emoji-text (substring (match-string 0) 1 -1))
         match)
    (if (or (nth 3 syntax-ppss-at-point)
            (nth 4 syntax-ppss-at-point))
        (let ((emoji-one (gethash emoji-text emoji-parsed)))
          (when emoji-one
            (add-text-properties start end (list 'display (create-image (expand-file-name (concat (gethash "unicode" emoji-one) ".png")
                                                                                          emoji-image-dir)
                                                                        ;; Use imagemagick if available (allows resizing images)
                                                                        (when (fboundp 'imagemagick-types)
                                                                          'imagemagick)
                                                                        nil
                                                                        :ascent 'center
                                                                        ;; no-op if imagemagick is not available
                                                                        :height (default-font-height))))))

      (remove-text-properties start end '(display))))
  nil)

(defun emoji-make-keywords ()
  (let ((emojis (mapcar (lambda (word) (concat ":" word ":")) (hash-table-keys emoji-parsed))))
    `((,(regexp-opt emojis)
       (0 (emoji-setup-emoji-display))))))

(defvar emoji-make-keywords (emoji-make-keywords))

(define-minor-mode emojify-mode
  "Emojify mode"
  :init-value nil
  ;; Do not bother non font-lock buffers
  (when font-lock-major-mode
    (if emojify-mode
        ;; Turn on
        (progn
          (font-lock-add-keywords nil emoji-make-keywords)
          (setq-local font-lock-extra-managed-props
                      (cons 'display font-lock-extra-managed-props)))
      ;; Turn off
      (font-lock-remove-keywords nil emoji-make-keywords)
      (setq font-lock-extra-managed-props (delq 'composition
                                                font-lock-extra-managed-props)))
    (font-lock-ensure)))

(defun turn-on-emojify-mode ()
  (emojify-mode 1))

(define-globalized-minor-mode global-emojify-mode
  emojify-mode turn-on-emojify-mode)
