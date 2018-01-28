;; A simple configuration file to launch a standalone instance of Emacs
;; with just emojify and it's dependencies installed
;;
;; Do something like the following
;;
;; cask exec emacs -Q -l test/launch.el
;;
(let ((project-dir (locate-dominating-file (or (buffer-file-name) load-file-name)
                                           ".cask")))
  (if (not project-dir)
      (user-error "Could not locate project root")
    (let ((default-directory (expand-file-name (format ".cask/%d.%d"
                                                       emacs-major-version
                                                       emacs-minor-version)
                                               project-dir)))
      (normal-top-level-add-subdirs-to-load-path))
    (add-to-list 'load-path project-dir)))

(require 'emojify)

(setq emojify-emojis-dir (make-temp-file "emojify" t))
