;;; emojify.el --- Display emojis in Emacs           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Iqbal Ansari

;; Author: Iqbal Ansari <iqbalansari02@yahoo.com>
;; Keywords: multimedia, convenience
;; Version: 0.1
;; Package-Requires: ((seq "1.12") (ht "2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Another plugin to waste time in Emacs :sweat: :worried: :unamused: :p :) :p



;;; Code:

(require 'seq)
(require 'ht)

(require 'json)
(require 'regexp-opt)
(require 'jit-lock)
(require 'pcase)



;; Satisfying the byte-compiler
;; We do not "require" these functions but if `org-mode' is active we use them

;; Required to determine point is in an org-list
(declare-function org-at-item-p "org-list")

;; Required to determine point is in an org-src block
(declare-function org-element-type "org-element")
(declare-function org-element-at-point "org-element")



;; Customizations for control how emojis are displayed

(defgroup emojify nil
  "Customization options for mu4e-alert"
  :group 'display
  :prefix "emojify-")

(defcustom emojify-emoji-json
  (expand-file-name "emoji.json" (if load-file-name
                                     (file-name-directory load-file-name)
                                   default-directory))
  "The path to JSON file containing the configuration for displaying emojis."
  :type 'file
  :group 'emojify)

(defcustom emojify-image-dir
  (expand-file-name "images" (if load-file-name
                                 (file-name-directory load-file-name)
                               default-directory))
  "Path to the directory containing the image."
  :type 'directory
  :group 'emojify)

(defcustom emojify-substitution-style
  'image
  "How the emoji's be displayed.

Possible values are
`image'   - Display emojis using images, this requires images are supported by
            user's Emacs installation
`unicode' - Display emojis using unicode codepoints, this works well on
            platforms with good emoji fonts.  In this case the emoji text
            ':wink:' will be substituted with 😉.
`ascii'   - Display emojis as ascii charcters, this is simplest and does not
            require any external dependencies.  In this cases emoji text like
            ':wink:' are substituted with ascii equivalents like ';)'"
  :group 'emojify)



;; Customizations to control the enabling of emojify-mode

(defcustom emojify-inhibit-major-modes
  '(dired-mode
    doc-view-mode
    pdf-view-mode
    image-mode
    help-mode
    magit-popup-mode)
  "Major modes where emojify mode should not be enabled."
  :type '(repeat symbol)
  :group 'emojify)

(defcustom emojify-inhibit-in-buffer-functions
  '(minibufferp emojify-helm-buffer-p)
  "Functions used to determine emojify-mode should be enabled in a buffer.

These functions are called with one argument, the buffer where emojify-mode
is about to be enabled, emojify is not enabled if any of the functions return
a non-nil value."
  :type 'hook
  :group 'emojify)

(defun emojify-ephemeral-buffer-p (buffer)
  "Determine if BUFFER is an ephemeral/temporary buffer."
  (string-match-p "^ " (buffer-name buffer)))

(defun emojify-inhibit-major-mode-p (buffer)
  "Determine if user has disabled the `major-mode' enabled for the BUFFER.

Returns non-nil if the buffer's major mode is part of `emojify-inhibit-major-modes'"
  (memq (with-current-buffer buffer
          major-mode)
        emojify-inhibit-major-modes))

(defun emojify-helm-buffer-p (buffer)
  "Determine if the current BUFFER is a helm buffer."
  (string-match-p "\\*helm" (buffer-name buffer)))

(defun emojify-buffer-p (buffer)
  "Determine if `emojify-mode' should be enabled for given BUFFER.

`emojify-mode' mode is not enabled in temporary buffers.  Additionally user
can customize `emojify-inhibit-major-modes' and
`emojify-inhibit-in-buffer-functions' to disabled emojify in additional buffers."
  (not (or (emojify-ephemeral-buffer-p (current-buffer))
           (emojify-inhibit-major-mode-p (current-buffer))
           (buffer-base-buffer buffer)
           (run-hook-with-args-until-success 'emojify-inhibit-in-buffer-functions buffer))))



;; Customizations to control display of emojis

(defvar emojify-emojis nil
  "Data about the emojis, this contains only the emojis that come with emojify.")

(defvar emojify-regexps nil
  "Regexp to match text to emojified.")

(defun emojify-set-emoji-data ()
  "Read the emoji data and set the regexp required to search them."
  (setq emojify-emojis (let ((json-array-type 'list)
                             (json-object-type 'hash-table))
                         (json-read-file emojify-emoji-json)))

  (unless (eq emojify-preferred-style 'all)
    (let ((style (symbol-name emojify-preferred-style)))
      (ht-reject! (lambda (key value)
                    (not (string= style (ht-get value "style"))))
                  emojify-emojis)))

  (setq emojify-regexps (let ((emojis (ht-keys emojify-emojis)))
                          (regexp-opt emojis))))

;;;###autoload
(defun emojify-set-preferred-style (value)
  "Set the type of emojis that should be displayed.

VALUE is the value to be used as preferred style, see `emojify-preferred-style'"
  (let ((style (if (consp value)
                   (eval value)
                 value)))
    (unless (memq style '(all github ascii))
      (user-error "[emojify] Do not know how to use `%s' style, please set to one of %s"
                  value
                  '(all github ascii)))
    (setq-default emojify-preferred-style style)
    (emojify-set-emoji-data)))

(defcustom emojify-preferred-style
  'all
  "The type of emojis that should be displayed.

These can have one of the following values

`ascii'  - Display only ascii emojis for example ';)'
`github' - Display only github style emojis for example ':wink:'
`all'    - Display all of the above"
  :type '(radio :tag "Emoji Style"
                (const :tag "Display only ascii emojis" ascii)
                (const :tag "Display only github emojis" github)
                (const :tag "Display all emojis" all))
  :set (lambda (_ value) (emojify-set-preferred-style value)))

(defcustom emojify-prog-contexts
  'both
  "Contexts where emojis can be displayed in programming modes.

Possible values are
`comments' - Display emojis only in comments
`string'   - Display emojis only in strings
`both'     - Display emojis in comments and strings
`none'     - Do not display emojis in programming modes"
  :group 'emojify)

(defcustom emojify-inhibit-functions
  '(emojify-in-org-tags-p emojify-in-org-list-p)
  "Functions used to determine given emoji should displayed at current point.

These functions are called with 3 arguments, the text to be emojified, the start
of emoji text and the end of emoji text.  These functions are called with the
buffer where emojis are going to be displayed selected."
  :type 'hook
  :group 'emojify)

(defun emojify-in-org-tags-p (match beg end)
  "Determine whether the point is on `org-mode' tag.

MATCH, BEG and END are the text currently matched emoji and the start position
and end position of emoji text respectively."
  (and (eq major-mode 'org-mode)
       (string-match-p ":.*:" match)
       (eq (line-end-position) end)))

(defun emojify-in-org-list-p (&rest ignored)
  "Determine whether the point is in `org-mode' list.

The arguments IGNORED are, well ignored"
  (and (eq major-mode 'org-mode)
       (org-at-item-p)))

(defun emojify-valid-prog-context-p (beg end)
  "Determine if the text between BEG and END should be used to display emojis.

This returns non-nil if the region is valid according to `emojify-prog-contexts'"
  (unless (or (not emojify-prog-contexts)
          (eq emojify-prog-contexts 'none))
    (let ((syntax-beg (syntax-ppss beg))
          (syntax-end (syntax-ppss end))
          (pos (pcase emojify-prog-contexts
                 (`string 3)
                 (`comments 4)
                 (`both 8))))
      (and (nth pos syntax-beg)
           (nth pos syntax-end)))))

(defun emojify-inside-org-src-p (point)
  "Return non-nil if POINT is inside `org-mode' src block.

This is used to inhibit display of emoji's in `org-mode' src blocks
since our mechanisms do not work in it."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char point)
      (eq (org-element-type (org-element-at-point)) 'src-block))))

(defun emojify-valid-text-context-p (beg end)
  "Determine if the okay to display for text between BEG and END."
  ;; The text is at the start of the buffer
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
                   ;; punctuation
                   ?.
                   ;; comment end
                   ?>)))))



;; Customizations to control the behaviour when point enters emojified text

(defcustom emojify-point-entered-behaviour 'echo
  "The behaviour when point enters, an emojified text.

It can be one of the following
`echo'    - Echo the underlying text in the minibuffer
`uncover' - Display the underlying text while point is on it
function  - It is called with 4 arguments
            1) buffer where emoji text is
            2) the emoji text
            3) starting position of emoji text
            4) ending position of emoji text

Does nothing if the value is anything else."
  :group 'emojify)

(defcustom emojify-show-help t
  "If non-nil the underlying text is displayed in a popup when mouse moves over it."
  :group 'emojify)

(defun emojify--uncover-emoji (buffer match-beginning match-end)
  "Uncover emoji in BUFFER between MATCH-BEGINNING and MATCH-END."
  (progn (emojify-undisplay-emojis-in-region match-beginning match-end)
         (with-silent-modifications
           (add-text-properties match-end
                                match-beginning
                                (list 'point-left (emojify--get-point-left-function buffer
                                                                                    match-beginning
                                                                                    match-end))))))

(defun emojify-point-entered-function (old-point new-point)
  "Create a function to be executed when point enters an emojified text.

OLD-POINT and NEW-POINT are the point before entering and after entering."
  (let* ((text-props (text-properties-at new-point))
         (buffer (plist-get text-props 'emojify-buffer))
         (match (plist-get text-props 'emojify-text))
         (match-beginning (plist-get text-props 'emojify-start))
         (match-end (plist-get text-props 'emojify-end)))
    (when (eq buffer (current-buffer))
      (cond ((and (eq emojify-point-entered-behaviour 'echo)
                  ;; Do not echo in isearch-mode
                  (not isearch-mode))
             (message (substring-no-properties match)))
            ((eq emojify-point-entered-behaviour 'uncover)
             (emojify--uncover-emoji buffer match-beginning match-end))
            ((functionp 'emojify-point-entered-behaviour)
             (funcall emojify-point-entered-behaviour buffer match match-beginning match-end)))

      ;; Uncover at point anyway in isearch-mode
      (when (and isearch-mode
                 (not (eq emojify-point-entered-behaviour 'uncover)))
        (emojify--uncover-emoji buffer match-beginning match-end)))))

(defun emojify-help-function (window string pos)
  "Function to get help string to be echoed when point/mouse into the point.

To understand WINDOW, STRING and POS see the function documentation for
`help-echo' text-property."
  (when emojify-show-help
    (plist-get (text-properties-at pos) 'emojify-text)))



;; Core functions and macros

(defun emojify--get-point-left-function (buffer match-beginning match-end)
  "Create a function that can be executed in point-left hook for emoji text.

BUFFER is the buffer where the text is from, MATCH-BEGINNING and MATCH-END.
mark the start and end of region containing the text."
  (lambda (old-point new-point)
    (when (and (equal buffer (current-buffer))
               (or (< match-end new-point)
                   (< new-point match-beginning)))
      (emojify-display-emojis-in-region match-beginning match-end))))

(defun emojify--get-image-display (data)
  "Get the display text property to display the emoji specified in DATA as an image."
  (let ((image-file (expand-file-name (ht-get data "image")
                                      emojify-image-dir)))
    (when (file-exists-p image-file)
      (create-image image-file
                    ;; use imagemagick if available (allows resizing images
                    (when (fboundp 'imagemagick-types)
                      'imagemagick)
                    nil
                    :ascent 'center
                    ;; no-op if imagemagick is not available
                    :height (default-font-height)))))

(defun emojify--get-unicode-display (data)
  "Get the display text property to display the emoji specified in DATA as unicode characters."
  (let* ((unicode (ht-get data "unicode"))
         (characters (when unicode
                       (seq-map (lambda (hex)
                                  (string-to-number hex 16))
                                (split-string unicode "-")))))
    (when (seq-every-p #'char-displayable-p characters)
      (seq-mapcat #'char-to-string characters 'string))))

(defun emojify--get-ascii-display (data)
  "Get the display text property to display the emoji specified in DATA as ascii characters."
  (ht-get data "ascii"))

(defun emojify--get-text-display-props (name)
  "The the display property for an emoji named NAME."
  (let* ((emoji-data (ht-get emojify-emojis name))
         (display (when emoji-data
                    (pcase emojify-substitution-style
                      (`image (emojify--get-image-display emoji-data))
                      (`unicode (emojify--get-unicode-display emoji-data))
                      (`ascii (emojify--get-ascii-display emoji-data))))))
    (when display
      (list 'display display))))

(defmacro emojify-with-saved-buffer-state (&rest forms)
  "Execute FORMS saving current buffer state.

This saves point and mark, `match-data' and buffer modification state it also
inhibits buffer change, point motion hooks.

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
    (while (and (> end (point))
                (search-forward-regexp emojify-regexps end t))
      (let ((match-beginning (match-beginning 0))
            (match-end (match-end 0))
            (match (match-string-no-properties 0))
            (buffer (current-buffer)))

        ;; Display unconditionally in non-prog mode
        (when (and (or (not (derived-mode-p 'prog-mode))
                       ;; In prog mode enable respecting `emojify-prog-contexts'
                       (emojify-valid-prog-context-p match-beginning match-end))

                   ;; The text is at the beginning of the buffer
                   (emojify-valid-text-context-p match-beginning match-end)

                   (not (emojify-inside-org-src-p match-beginning))

                   ;; Inhibit possibly inside a list
                   ;; 41 is ?) but packages get confused by the extra closing paren
                   ;; TODO Report bugs to such packages
                   (not (and (eq (char-syntax (char-before match-end)) 41)
                             (ignore-errors (scan-sexps match-end -1))))

                   (not (run-hook-with-args-until-success 'emojify-inhibit-functions match match-beginning match-end)))

          (let ((display-props (emojify--get-text-display-props match)))
            (when display-props
              (add-text-properties match-beginning
                                   match-end
                                   (append display-props
                                           (list 'emojified t
                                                 'emojify-buffer buffer
                                                 'emojify-text match
                                                 'emojify-start match-beginning
                                                 'emojify-end match-end
                                                 'point-entered #'emojify-point-entered-function
                                                 'help-echo #'emojify-help-function))))))))))

(defun emojify-undisplay-emojis-in-region (beg end)
  "Undisplay the emojis in region.

BEG and END are the beginning and end of the region respectively"
  (emojify-with-saved-buffer-state
    (while (< beg end)
      ;; Get the start of emojified region in the region, the region is marked
      ;; with text-property `emojified' whose value is `t'. The region is marked
      ;; so that we do not inadvertently remove display or other properties
      ;; inserted by other packages.  This might fail too if a package adds any
      ;; of these properties between an emojified text, but that situation is
      ;; hopefully very rare and this is better than blindly removing all text
      ;; properties
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
                                                                      'point-entered t
                                                                      'point-left t
                                                                      'emojify-buffer t
                                                                      'emojify-text t
                                                                      'emojify-start t
                                                                      'emojify-end t
                                                                      'help-echo t))))
        ;; Setup the next iteration
        (setq beg emoji-end)))))

(defun emojify-after-change-function (beg end len)
  "Redisplay emojis in region after change.

This functions is added to `after-change-functions'.  See documentation
of `after-change-functions' to understand the meaning of BEG, END and LEN."
  (let ((inhibit-read-only t)
        ;; Extend the region to match the beginning of line where change began
        (region-end (save-excursion
                      (goto-char end)
                      (line-end-position)))
        ;; Extend the region to match the end of line where change ended
        (region-start (save-excursion
                        (goto-char beg)
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
  "Turn on `emojify-mode' in current buffer."
  (unless (and emojify-emojis emojify-regexps)
    (emojify-set-emoji-data))

  (when (emojify-buffer-p (current-buffer))
    (if font-lock-defaults
        ;; Use jit-lock if available, it is much better for larger
        ;; buffers than our brute force display
        (jit-lock-register #'emojify-display-emojis-in-region)
      (save-restriction
        (widen)
        (emojify-display-emojis-in-region (point-min) (point-max))))

    ;; An after change hook is added irrespective of whether jit-lock is used
    ;; because we do not rely of font-locks mechanism for clearing the `display'
    ;; when text is deleted and thus should do it manually
    (add-hook 'after-change-functions #'emojify-after-change-function t t)))

(defun emojify-turn-off-emojify-mode ()
  "Turn off `emojify-mode' in current buffer."
  ;; Remove currently displayed emojis
  (save-restriction
    (widen)
    (emojify-undisplay-emojis-in-region (point-min) (point-max)))

  (jit-lock-unregister #'emojify-display-emojis-in-region)

  ;; Uninstall our after change function
  (remove-hook 'after-change-functions #'emojify-after-change-function t))

(define-minor-mode emojify-mode
  "Emojify mode"
  :init-value nil
  (if emojify-mode
      ;; Turn on
      (emojify-turn-on-emojify-mode)
    ;; Turn off
    (emojify-turn-off-emojify-mode)))

;; While this is convenient it is not really what we want
(define-globalized-minor-mode global-emojify-mode
  emojify-mode emojify-turn-on-emojify-mode
  :init-value nil)

(provide 'emojify)
;;; emojify.el ends here
