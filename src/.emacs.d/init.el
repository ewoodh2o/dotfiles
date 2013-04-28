;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el
;;; Elliott Wood <elliott@two-fish.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)
(nconc load-path (list "~/.emacs.d/el-get/el-get"))
;; (add-to-list 'load_path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (let ((buf
          (url-retrieve-synchronously
            "https://raw.github.com/dimitri/el-get/master/el-get-install.el")))
    (if buf
      (with-current-buffer buf
        (let (el-get-master-branch)
          (end-of-buffer)
          (eval-print-last-sexp))))))

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq el-get-sources
  '((:name scss-mode
       :type git
       :url "http://github.com/antonj/scss-mode.git")
     (:name css-mode
       :type elpa)
     (:name rvm
       :type git
       :url "http://github.com/senny/rvm.el.git"
       :load "rvm.el"
       :compile ("rvm.el"))
     (:name rainbow-mode
       :type elpa)
     (:name zenburn
       :type http
       :url "http://github.com/djcb/elisp/raw/master/themes/zenburn-theme.el"
       :compile ("zenburn-theme.el"))
     (:name flycheck
       :type elpa)))

(defvar required-packages
  '(auto-complete
    auto-complete-css
    coffee-mode
    flycheck
    go-mode
    haml-mode
    iedit
    markdown-mode
    rainbow-mode
    rhtml-mode
    rvm
    ;; rhtml
    ;; yasnippet
    ruby-mode
    css-mode
    scss-mode
    ;slime
    yaml-mode
    zenburn))

;; override notifications to be displayed in the message buffer if
;; we're running in a terminal.
(unless (display-graphic-p)
  (defun el-get-notify (title msg)
           (message "%s: %s" title msg)))

(el-get 'sync required-packages)

;; turn on autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Load the correct ruby based on RVM's default setting
(rvm-activate-corresponding-ruby)

(defun load-user-file (file)
  (interactive "f")
  "Load a file in the user's current configuration directory."
  (load-file (expand-file-name file
               (expand-file-name "config" user-emacs-directory))))

;; store emacs auto-customization in its own file.
(setq custom-file (expand-file-name "auto-custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; load user customization
(load-user-file "paths.el")
(load-user-file "ui.el")
(load-user-file "behavior.el")
(load-user-file "modes.el")
(load-user-file "ruby.el")
(load-user-file "js.el")
;(load-user-file "slime.el")
(load-user-file "move-text.el")
(load-user-file "keybinds.el")
(load-user-file "textmate.el")

;; enable textmate mode
(textmate-mode)

;; display startup timing after load
(fset 'startup-echo-area-message
  '(lambda ()
     (message "emacs loaded in %s" (emacs-init-time))))
