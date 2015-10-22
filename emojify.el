;; Another plugin to waste time in Emacs :sweat: :worried: :unamused:

(require 'json)
(require 'subr-x)

(defvar emoji-emoji-json (expand-file-name "images" (file-name-directory load-file-name)))
(defvar emoji-image-dir (expand-file-name "images" (file-name-directory load-file-name)))
(defvar emoji-parsed (let ((json-array-type 'list)
                           (json-object-type 'hash-table))
                       (json-read-file emoji-emoji-json)))

(defun emoji-compose-symbol ()
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
                                                                        nil
                                                                        nil
                                                                        :ascent 'center)))))
      (remove-text-properties start end '(display))))
  nil)



(defun emoji-make-keywords ()
  (let ((emojis (mapcar (lambda (word) (concat ":" word ":")) (hash-table-keys emoji-parsed))))
    `((,(regexp-opt emojis)
       (0 (emoji-compose-symbol))))))

(font-lock-add-keywords nil (emoji-make-keywords))
(setq-local font-lock-extra-managed-props
            (cons 'display font-lock-extra-managed-props))
