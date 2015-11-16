;;; emojify.el --- Display emojis in Emacs           -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Iqbal Ansari

;; Author: Iqbal Ansari <iqbalansari02@yahoo.com>
;; Keywords: multimedia, convenience
;; URL: https://github.com/iqbalansari/emacs-emojify
;; Version: 0.2
;; Package-Requires: ((seq "1.12") (ht "2.0") (emacs "24.3"))

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

;; This package displays emojis in Emacs similar to how Github, Slack etc do. It
;; can display plain ascii like ':)' as well as Github style emojis like ':smile:'
;;
;; It provides a minor mode `emojify-mode' to enable display of emojis in a buffer.
;; To enable emojify mode globally use `global-emojify-mode'
;;
;; For detailed documentation see the projects README file at
;; https://github.com/iqbalansari/emacs-emojify



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
(declare-function org-at-heading-p "org")

;; Required to determine point is in an org-src block
(declare-function org-element-type "org-element")
(declare-function org-element-at-point "org-element")



;; Compatibility functions

(defun emojify-default-font-height ()
  "Return the height in pixels of the current buffer's default face font.

`default-font-height' seems to be available only on Emacs versions after 24.3.
This provides a compatibility version for previous versions."
  (if (fboundp 'default-font-height)
      (default-font-height)
    (let ((default-font (face-font 'default)))
      (cond
       ((and (display-multi-font-p)
             ;; Avoid calling font-info if the frame's default font was
             ;; not changed since the frame was created.  That's because
             ;; font-info is expensive for some fonts, see bug #14838.
             (not (string= (frame-parameter nil 'font) default-font)))
        (aref (font-info default-font) 3))
       (t (frame-char-height))))))



;; Utility functions

(defun emojify--get-relevant-region ()
  "Try getting region in buffer that completely covers the current window."
  (let* ((window-size (- (window-end) (window-start)))
         (start (max (- (point) window-size) (point-min)))
         (end (min (+ (point) window-size) (point-max))))
    (cons start end)))



;; Customizations for control how emojis are displayed

(defgroup emojify nil
  "Customization options for emojify"
  :group 'display
  :prefix "emojify-")

(defcustom emojify-emoji-json
  (expand-file-name "data/emoji.json" (if load-file-name
                                     (file-name-directory load-file-name)
                                   default-directory))
  "The path to JSON file containing the configuration for displaying emojis."
  :type 'file
  :group 'emojify)

(defcustom emojify-image-dir
  (expand-file-name "images" (if load-file-name
                                 (file-name-directory load-file-name)
                               default-directory))
  "Path to the directory containing the emoji images."
  :type 'directory
  :group 'emojify)

(defcustom emojify-display-style
  'image
  "How the emoji's be displayed.

Possible values are
`image'   - Display emojis using images, this requires images are supported by
            user's Emacs installation
`unicode' - Display emojis using unicode characters, this works well on
            platforms with good emoji fonts.  In this case the emoji text
            ':wink:' will be substituted with 😉.
`ascii'   - Display emojis as ascii characters, this is simplest and does not
            require any external dependencies.  In this cases emoji text like
            ':wink:' are substituted with ascii equivalents like ';)'"
  :type '(radio :tag "Emoji display style"
                (const :tag "Display emojis as images" image)
                (const :tag "Display emojis as unicode characters" unicode)
                (const :tag "Display emojis as ascii string" ascii))
  :group 'emojify)



;; Customizations to control the enabling of emojify-mode

(defcustom emojify-inhibit-major-modes
  '(dired-mode
    doc-view-mode
    pdf-view-mode
    image-mode
    help-mode
    magit-popup-mode
    ert-results-mode
    compilation-mode
    proced-mode
    comint-mode
    mu4e-headers-mode)
  "Major modes where emojify mode should not be enabled."
  :type '(repeat symbol)
  :group 'emojify)

(defcustom emojify-inhibit-in-buffer-functions
  '(minibufferp emojify-helm-buffer-p)
  "Functions used inhibit emojify-mode in a buffer.

These functions are called with one argument, the buffer where emojify-mode
is about to be enabled, emojify is not enabled if any of the functions return
a non-nil value."
  :type 'hook
  :group 'emojify)

(defvar emojify-inhibit-emojify-in-current-buffer-p nil
  "Should emojify be inhibited in current buffer.

This is a buffer local variable that can be set to inhibit enabling of
emojify in a buffer.")
(make-variable-buffer-local 'emojify-inhibit-emojify-in-current-buffer-p)

(defun emojify-ephemeral-buffer-p (buffer)
  "Determine if BUFFER is an ephemeral/temporary buffer."
  (string-match-p "^ " (buffer-name buffer)))

(defun emojify-inhibit-major-mode-p (buffer)
  "Determine if user has disabled the `major-mode' enabled for the BUFFER.

Returns non-nil if the buffer's major mode is part of `emojify-inhibit-major-modes'"
  (with-current-buffer buffer
    (apply #'derived-mode-p emojify-inhibit-major-modes)))

(defun emojify-helm-buffer-p (buffer)
  "Determine if the current BUFFER is a helm buffer."
  (string-match-p "\\*helm" (buffer-name buffer)))

(defun emojify-buffer-p (buffer)
  "Determine if `emojify-mode' should be enabled for given BUFFER.

`emojify-mode' mode is not enabled in temporary buffers.  Additionally user
can customize `emojify-inhibit-major-modes' and
`emojify-inhibit-in-buffer-functions' to disabled emojify in additional buffers."
  (not (or emojify-inhibit-emojify-in-current-buffer-p
           (emojify-ephemeral-buffer-p (current-buffer))
           (emojify-inhibit-major-mode-p (current-buffer))
           (buffer-base-buffer buffer)
           (run-hook-with-args-until-success 'emojify-inhibit-in-buffer-functions buffer))))



;; Customizations to control display of emojis

(defvar emojify-emojis nil
  "Data about the emojis, this contains only the emojis that come with emojify.")

(defvar emojify-regexps nil
  "Regexp to match text to emojified.")

(defvar emojify-emoji-style-change-hooks nil
  "Hooks run when emoji style changes.")

(defun emojify-set-emoji-data (styles)
  "Read the emoji data for STYLES and set the regexp required to search them."
  (setq emojify-emojis (let ((json-array-type 'list)
                             (json-object-type 'hash-table))
                         (json-read-file emojify-emoji-json)))

  (ht-reject! (lambda (_key value)
                (not (memq (intern (ht-get value "style")) styles)))
              emojify-emojis)

  (setq emojify-regexps (when (ht-keys emojify-emojis)
                          (let ((emojis (ht-keys emojify-emojis)))
                            (regexp-opt emojis)))))

;;;###autoload
(defun emojify-set-emoji-styles (styles)
  "Set the type of emojis that should be displayed.

STYLES is the styles emoji styles that should be used, see `emojify-emoji-styles'"
  (when (not (listp styles))
    (setq styles (list styles))
    (warn "`emojify-emoji-style' has been deprecated use `emojify-emoji-styles' instead!"))

  (setq-default emojify-emoji-styles styles)

  ;; Update emoji data
  (emojify-set-emoji-data styles)

  (run-hooks 'emojify-emoji-style-change-hooks))

(defcustom emojify-emoji-styles
  '(ascii unicode github)
  "The type of emojis that should be displayed.

These can have one of the following values

`ascii'   - Display only ascii emojis for example ';)'
`unicode' - Display only unicode emojis for example '😉'
`github'  - Display only github style emojis for example ':wink:'"
  :type '(set
          (const :tag "Display only ascii emojis" ascii)
          (const :tag "Display only github emojis" github)
          (const :tag "Display only unicode codepoints" unicode))
  :set (lambda (_ value) (emojify-set-emoji-styles value))
  :group 'emojify)

;; Obsolete vars
(define-obsolete-variable-alias 'emojify-emoji-style 'emojify-emoji-styles "0.2")
(define-obsolete-function-alias 'emojify-set-emoji-style 'emojify-set-emoji-styles "0.2")

(defcustom emojify-prog-contexts
  'both
  "Contexts where emojis can be displayed in programming modes.

Possible values are
`comments' - Display emojis only in comments
`string'   - Display emojis only in strings
`both'     - Display emojis in comments and strings
`none'     - Do not display emojis in programming modes"
  :type '(radio :tag "Contexts where emojis should be displayed in programming modes"
                (const :tag "Only in comments" comments)
                (const :tag "Only in string" string)
                (const :tag "Both in comments and string" both)
                (const :tag "Do not display emojis in programming modes" none))
  :group 'emojify)

(defcustom emojify-inhibit-functions
  '(emojify-in-org-tags-p emojify-in-org-list-p)
  "Functions used to determine given emoji should displayed at current point.

These functions are called with 3 arguments, the text to be emojified, the start
of emoji text and the end of emoji text.  These functions are called with the
buffer where emojis are going to be displayed selected."
  :type 'hook
  :group 'emojify)

(defun emojify-in-org-tags-p (match _beg _end)
  "Determine whether the point is on `org-mode' tag.

MATCH, BEG and END are the text currently matched emoji and the start position
and end position of emoji text respectively.

Easiest would have to inspect face at point but unfortunately, there is no
way to guarantee that we run after font-lock"
  (and (memq major-mode '(org-mode org-agenda-mode))
       (string-match-p ":.*:" match)
       (org-at-heading-p)
       (not (save-excursion
              (save-match-data
                (search-forward-regexp "\\s-" (line-end-position) t))))))

(defun emojify-in-org-list-p (&rest ignored)
  "Determine whether the point is in `org-mode' list.

The arguments IGNORED are, well ignored"
  (and (eq major-mode 'org-mode)
       (org-at-item-p)))

(defun emojify-valid-prog-context-p (beg end)
  "Determine if the text between BEG and END should be used to display emojis.

This returns non-nil if the region is valid according to `emojify-prog-contexts'"
  (when (and emojify-prog-contexts
             (memq emojify-prog-contexts '(string comments both)))
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

(defun emojify-looking-at-end-of-list-maybe (point)
  "Determine if POINT is end of a list.

This is not accurate since it restricts the region to scan to
the visible area."
  (let* ((area (emojify--get-relevant-region))
         (beg (car area))
         (end (cdr area)))
    (save-restriction
      (narrow-to-region beg end)
      (let ((list-start (ignore-errors (scan-sexps point -1))))
        (when list-start
          ;; If we got a list start make sure both start and end
          ;; belong to same string/comment
          (let ((syntax-beg (syntax-ppss list-start))
                (syntax-end (syntax-ppss point)))
            (and list-start
                 (eq (nth 8 syntax-beg)
                     (nth 8 syntax-end)))))))))

(defun emojify-valid-text-context-p (beg end)
  "Determine if the okay to display for text between BEG and END."
  ;; The text is at the start of the buffer
  (and (or (not (char-before beg))
           ;; 32 space since ?  (? followed by a space) is not readable
           ;; 34 is "  since?" confuses font-lock
           ;; 41 is )  since?) (extra paren) confuses most packages
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
                   ;; closing braces
                   41
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
  ;; TODO: Mention custom function
  :type '(radio :tag "Behaviour when point enters an emoji"
                (const :tag "Echo the underlying emoji text in the minibuffer" echo)
                (const :tag "Uncover (undisplay) the underlying emoji text" uncover))
  :group 'emojify)

(defcustom emojify-show-help t
  "If non-nil the underlying text is displayed in a popup when mouse moves over it."
  :type 'boolean
  :group 'emojify)

(defun emojify--uncover-emoji (buffer match-beginning match-end)
  "Uncover emoji in BUFFER between MATCH-BEGINNING and MATCH-END."
  (progn (emojify-undisplay-emojis-in-region match-beginning match-end)
         (with-silent-modifications
           (add-text-properties match-end
                                match-beginning
                                (list 'point-left (emojify--get-point-left-function buffer
                                                                                    match-beginning
                                                                                    match-end)
                                      'emojified t)))))

(defun emojify-point-entered-function (_old-point new-point)
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
                  (not isearch-mode)
                  (not (active-minibuffer-window))
                  (not (current-message)))
             (message (substring-no-properties match)))
            ((eq emojify-point-entered-behaviour 'uncover)
             (emojify--uncover-emoji buffer match-beginning match-end))
            ((functionp 'emojify-point-entered-behaviour)
             (funcall emojify-point-entered-behaviour buffer match match-beginning match-end)))

      ;; Uncover at point anyway in isearch-mode
      (when (and isearch-mode
                 (not (eq emojify-point-entered-behaviour 'uncover)))
        (emojify--uncover-emoji buffer match-beginning match-end)))))

(defun emojify-help-function (_window _string pos)
  "Function to get help string to be echoed when point/mouse into the point.

To understand WINDOW, STRING and POS see the function documentation for
`help-echo' text-property."
  (when (and emojify-show-help
             (not isearch-mode)
             (not (active-minibuffer-window))
             (not (current-message)))
    (plist-get (text-properties-at pos) 'emojify-text)))



;; Core functions and macros

(defun emojify--get-point-left-function (buffer match-beginning match-end)
  "Create a function that can be executed in point-left hook for emoji text.

BUFFER is the buffer where the text is from, MATCH-BEGINNING and MATCH-END.
mark the start and end of region containing the text."
  (lambda (_old-point new-point)
    (when (and (equal buffer (current-buffer))
               (or (< match-end new-point)
                   (< new-point match-beginning)))
      (emojify-redisplay-emojis match-beginning match-end))))

(defun emojify--get-image-display (data)
  "Get the display text property to display the emoji specified in DATA as an image."
  (let* ((image-file (expand-file-name (ht-get data "image")
                                       emojify-image-dir))
         (image-type (intern (upcase (file-name-extension image-file)))))
    (when (file-exists-p image-file)
      (create-image image-file
                    ;; use imagemagick if available and supports PNG images
                    ;; (allows resizing images)
                    (when (and (fboundp 'imagemagick-types)
                               (memq image-type (imagemagick-types)))
                      'imagemagick)
                    nil
                    :ascent 'center
                    ;; no-op if imagemagick is not available
                    :height (emojify-default-font-height)))))

(defun emojify--get-unicode-display (data)
  "Get the display text property to display the emoji specified in DATA as unicode characters."
  (let* ((unicode (ht-get data "unicode"))
         (characters (when unicode
                       (string-to-vector unicode))))
    (when (seq-every-p #'char-displayable-p characters)
      unicode)))

(defun emojify--get-ascii-display (data)
  "Get the display text property to display the emoji specified in DATA as ascii characters."
  (ht-get data "ascii"))

(defun emojify--get-text-display-props (name)
  "The the display property for an emoji named NAME."
  (let* ((emoji-data (ht-get emojify-emojis name))
         (display (when emoji-data
                    (pcase emojify-display-style
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
           (save-restriction
             (widen)
             ,@forms))))))

(defun emojify-display-emojis-in-region (beg end)
  "Display emojis in region.

BEG and END are the beginning and end of the region respectively.
TODO: Skip emojifying if region is already emojified."
  (emojify-with-saved-buffer-state
    (goto-char beg)
    (when emojify-regexps
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
                     ;; 41 is ?) but packages get confused by the extra closing paren :)
                     ;; TODO Report bugs to such packages
                     (not (and (eq (char-syntax (char-before match-end)) 41)
                               (emojify-looking-at-end-of-list-maybe match-end)))

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
                                                   'help-echo #'emojify-help-function)))))))))))

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

(defun emojify-redisplay-emojis (&optional beg end)
  "Redisplay emojis in region between BEG and END.

Redisplay emojis in the visible region if BEG and END are not specified"
  (let* ((area (emojify--get-relevant-region))
         (beg (or beg (car area)))
         (end (or end (cdr area))))
    (emojify-undisplay-emojis-in-region beg end)
    (emojify-display-emojis-in-region beg end)))

(defun emojify-after-change-function (beg end _len)
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

    (emojify-redisplay-emojis region-start region-end)))



(defadvice text-scale-increase (after emojify-resize-emojis (&rest ignored))
  "Advice `text-scale-increase' to trigger resizing of emojis on resize."
  (when emojify-mode
    (emojify-redisplay-emojis)))

(ad-activate #'text-scale-increase)

(defadvice isearch-repeat (around emojify-redisplay-after-isearch-left (direction))
  "Advice `isearch-repeat' to run emojify's point motion hooks.

By default isearch disables point-motion hooks while repeating (see
`isearch-invisible') breaking emojify's uncovering logic, this advice explicitly
runs (only emojify's) point motion hooks."
  (let ((old-pos (point)))
    (prog1 ad-do-it
      (when emojify-mode
        (let ((old-pos-props (text-properties-at old-pos))
              (new-pos-props (text-properties-at (point))))
          (unless (equal old-pos (point))
            (when (and (plist-get old-pos-props 'emojified)
                       (plist-get old-pos-props 'point-left))
              (funcall (plist-get old-pos-props 'point-left) old-pos (point)))
            (when (and (plist-get new-pos-props 'emojified)
                       (plist-get new-pos-props 'point-entered))
              (funcall (plist-get new-pos-props 'point-entered) old-pos (point)))))))))


(ad-activate #'isearch-repeat)

(defun emojify-turn-on-emojify-mode ()
  "Turn on `emojify-mode' in current buffer."

  ;; Calculate emoji data if needed
  (unless emojify-emojis
    (emojify-set-emoji-data emojify-emoji-styles))

  (when (emojify-buffer-p (current-buffer))
    ;; Install our jit-lock function
    (jit-lock-register #'emojify-redisplay-emojis)

    ;; Add an after change hook to emojify regions on change
    (add-hook 'after-change-functions #'emojify-after-change-function t t)

    ;; Redisplay visible emojis when emoji style changes
    (add-hook 'emojify-emoji-style-change-hooks #'emojify-redisplay-emojis)))

(defun emojify-turn-off-emojify-mode ()
  "Turn off `emojify-mode' in current buffer."
  ;; Remove currently displayed emojis
  (save-restriction
    (widen)
    (emojify-undisplay-emojis-in-region (point-min) (point-max)))

  ;; Uninstall our jit-lock function
  (jit-lock-unregister #'emojify-redisplay-emojis)

  ;; Uninstall our after change function
  (remove-hook 'after-change-functions #'emojify-after-change-function t)

  ;; Remove style change hooks
  (remove-hook 'emojify-emoji-style-change-hooks #'emojify-redisplay-emojis))

;;;###autoload
(define-minor-mode emojify-mode
  "Emojify mode"
  :init-value nil
  (if emojify-mode
      ;; Turn on
      (emojify-turn-on-emojify-mode)
    ;; Turn off
    (emojify-turn-off-emojify-mode)))

;;;###autoload
(define-globalized-minor-mode global-emojify-mode
  emojify-mode emojify-mode
  :init-value nil)

(provide 'emojify)
;;; emojify.el ends here
