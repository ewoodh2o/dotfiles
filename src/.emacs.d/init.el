;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nconc load-path (list "~/.emacs.d/el-get/el-get"))

(require 'package)
(setq exec-path (cons "/usr/local/git/bin/" exec-path))

(unless (require 'el-get nil t)
  (with-current-buffer
    (url-retrieve-synchronously
      "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(nconc package-archives (list '("tromey" . "http://tromey.com/elpa/")))
(package-initialize)

(setq el-get-sources
  '((:name ruby-mode
      :type http
      :url "https://raw.github.com/ruby/ruby/trunk/misc/ruby-mode.el"
      :load "ruby-mode.el")
     (:name inf-ruby
       :type elpa)
     (:name css-mode
       :type elpa)
     (:name rvm
       :type git
       :url "http://github.com/senny/rvm.el.git"
       :load "rvm.el"
       :compile ("rvm.el")
       :after (lambda() (rvm-use-default)))
     (:name yaml-mode
       :type git
       :url "http://github.com/yoshiki/yaml-mode.git"
       :features yaml-mode)
     (:name rhtml
       :type git
       :url "http://github.com/eschulte/rhtml.git"
       :features rhtml-mode)
     (:name  scss-mode
       :type git
       :url "http://github.com/ewoodh2o/scss-mode.git"
       :features scss-mode)
     (:name yasnippet
       :type git
       :url "http://github.com/capitaomorte/yasnippet.git"
       :features yasnippet
       :after (lambda()
                (setq yas/snippet-dirs
                      (list "~/.emacs.d/el-get/yasnippet/snippets"))
                (yas/initialize)))
     (:name js2-mode
        :type git
        :url "https://github.com/mooz/js2-mode.git"
        :after (lambda()
                 (setq-default js2-consistent-level-indent-inner-bracket-p t)
                 (setq-default js2-pretty-multiline-decl-indentation-p t))
	:compile "js2-mode.el"
        :features js2-mode)
     (:name zenburn
       :type http
       :url "http://github.com/djcb/elisp/raw/master/themes/zenburn-theme.el"
       :compile ("zenburn-theme.el")
     )))

(defvar required-packages
  '(ruby-mode inf-ruby css-mode rvm yaml-mode rhtml haml-mode yasnippet
              scss-mode auto-complete-yasnippet  auto-complete-css
              auto-complete-emacs-lisp auto-complete js2-mode json lua-mode
              markdown-mode coffee-mode nxhtml
              zenburn
	      ))
; removed flymake-point and flymake-ruby
(el-get 'sync required-packages)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)
(require 'yasnippet)
(require 'uniquify)
;(require 'flymake-point)

(yas/load-directory "~/.emacs.d/el-get/yasnippet/snippets")
(yas/initialize)
(ac-config-default)

(setq auto-mode-alist
  (append
    (list
      '("Gemfile$"    . ruby-mode)
      '("Rakefile$"   . ruby-mode)
      '("\\.gemspec$" . ruby-mode)
      '("\\.ru$"      . ruby-mode)
      '("\\.rake$"    . ruby-mode)
      '("\\.erb$"     . rhtml-mode)
      '("\\.yaml$"    . yaml-mode)
      '("\\.yml$"     . yaml-mode)
      '("\\.coffee$"  . coffee-mode)
      '("Cakefile$"   . coffee-mode)
      '("\\.scss$"    . scss-mode)
      )
    auto-mode-alist))

; Stop dinging every time I hit the beginning or end of a buffer
(setq ring-bell-function (lambda () ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hexcolour-luminance (colour)
  "Calculate the luminance of a color string"
  (let* ((values (color-values colour))
          (r (car values))
          (g (cadr values))
          (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun hexcolour-add-to-font-lock ()
  "Colorize HTML RGB colors (e.g. '#000030', 'DarkBlue') in font-lock-mode."
  (interactive)
  (font-lock-add-keywords nil
    `((,(concat "#[0-9a-fA-F]\\{6\\}\\|"
          (regexp-opt (defined-colors) 'words))
        (0 (let ((colour (match-string-no-properties 0)))
             (put-text-property
               (match-beginning 0) (match-end 0)
               'face `((:foreground, (if (> 128.0 (hexcolour-luminance colour))
                                       "white" "black"))
                        (:background ,colour)))))))))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun toggle-fullscreen ()
  "Switch between fullscreen and windowed mode"
  (interactive)
  (if (eq system-type 'darwin)
      (ns-toggle-fullscreen)
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                             nil
                                           'fullboth))))

(defun scale-colour (colour factor)
  "Scale the given hex colour (#112233) by the given factor."
  (if window-system
      (let* ((values (color-values colour))
	     (r (floor (* factor (car values))))
	     (g (floor (* factor (cadr values))))
	     (b (floor (* factor (caddr values)))))
	(format "#%02x%02x%02x"
		(* (/ r 65280.0) 256)
		(* (/ g 65280.0) 256)
		(* (/ b 65280.0) 256)))
    colour))

(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquifiy' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-makealist iswitchb-default)
  (setq iswitchb-rescan t))

(defun iswitchb-local-keys ()
  "Use left/right to switch between buffer matches when using iswitchb"
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

(defun ew-indent()
  "*Indent code by two spaces."
  (interactive)
  (indent-rigidly (region-beginning) (region-end) 2))

(defun ew-outdent()
  "*Unindent code by two spaces."
  (interactive)
  (indent-rigidly (region-beginning) (region-end) -2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "s-/") 'comment-region)
(global-set-key (kbd "s-?") 'uncomment-region)
(global-set-key (kbd "s-[") 'ew-outdent)
(global-set-key (kbd "s-]") 'ew-indent)
(global-set-key (kbd "s-t") 'find-file-in-project)
(global-set-key (kbd "C-c C-r") 'align-repeat)
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key (kbd "M-RET") 'toggle-fullscreen)
(global-set-key (kbd "RET") 'newline-and-indent)

(if (eq system-type 'darwin)
    (global-set-key (kbd "C-<kp-delete>") 'kill-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default ansi-term-color-vector [unspecified "black" "red4" "lime green"
                                                  "yellow3" "DeepSkyBlue"
                                                  "magenta4" "cyan3" "white"])
(if (eq system-type 'darwin)
    (set-frame-font "Droid Sans Mono-12")
    (set-frame-font "Droid Sans Mono-10")
    (setq default-frame-alist '(
      (width . 140)
      (height . 50) )))


(setq frame-title-format   ;; frame title: user@host: buffer [modified?]
  (list
    (user-login-name)
    "@"
    (system-name)
    ": %b %+" ))


(setq line-number-mode t)              ; show line number in the mode line
(setq column-number-mode t)            ; show column number in the mode line
(which-function-mode t)                ; show current function in the mode line
(show-paren-mode t)                    ; highlight matching parentheses
(tool-bar-mode -1)                     ; no tool bar
(menu-bar-mode -1)                     ; no menu bar
(scroll-bar-mode -1)                   ; no scroll bar
(tooltip-mode -1)                      ; show tooltips in the echo area
(setq-default mumamo-chunk-coloring 2) ; don't highlight regions with terrible
                                       ; hideous colors
(load-theme 'wombat)
(set-face-background                   ; make trailing whitespace a little
 'trailing-whitespace                  ; darker than the default background
 (scale-colour
  (face-background 'default) 0.83))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)             ; ask y/n instead of yes/no
(setq inhibit-startup-message t)          ; I've used emacs before, thanks
(setq-default kill-whole-line t)          ; `C-k` also removes trailing \n
(setq-default require-final-newline nil)  ; don't require files to end with \n
(setq-default next-line-add-newlines nil) ; don't add newlines when scrolling
                                          ; past end of buffer
(setq-default show-trailing-whitespace t) ; highlight trailing whitespace
(setq-default tab-width 2)                ; default tab width is 2 spaces
(setq-default indent-tabs-mode nil)       ; use spaces instead of tabs to indent
(delete-selection-mode t)                 ; inserting text with a selection
                                          ; deletes the selection
(setq-default fill-column 80)             ; wrap text at 80 characters
(setq-default scroll-conservatively 1)    ; scroll one line at a time when the
                                          ; focus moves past end of buffer
(iswitchb-mode t)                         ; use better `C-x b` buffer switching
(put 'downcase-region 'disabled nil)      ; these are useful commands
(put 'upcase-region   'disabled nil)      ; why are they disabled

(setq uniquify-buffer-name-style 'forward); better uniquify buffer naming
(setq uniquify-after-kill-buffer-p t)     ; remove uniquify name after killing
                                          ; a competing buffer
(setq truncate-partial-width-windows nil) ; don't truncate lines in spit windows
(defvar backup-directory-location         ; save backup files in a non-annoying
  "~/.cache/emacs")                       ; directory location
(setq backup-directory-alist `((".*" . ,backup-directory-location)))
(setq auto-save-file-name-transforms `((".*" ,backup-directory-location t)))

(setq ac-auto-show-menu nil)              ; never pop up the autocomplete menu
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to autocomplete
(define-key ac-completing-map "\r" nil)   ; don't use return to autocomplete
(add-hook 'ruby-mode-hook                 ; don't autocomplete after "end"
  (lambda ()
    (make-local-variable 'ac-ignores)
      (add-to-list 'ac-ignores "end")))

; don't indent ruby stupidly
(setq-default ruby-deep-indent-paren nil)
(setq-default ruby-deep-indent-paren-style nil)
(setq-default ruby-deep-arglist nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook 'on-text-mode)
(add-hook 'c-mode-hook 'on-c-like-mode)
(add-hook 'lisp-mode-hook 'on-lisp-mode)
(add-hook 'js2-mode-hook 'on-js2-mode)
(add-hook 'emacs-lisp-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'nxml-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'scss-mode-hook 'on-scss-mode)

(defun on-text-mode ()
  (flyspell-mode t))

(defun on-c-like-mode ()
  (c-set-style "k&r")
  (c-set-offset 'substatement-open 0)
  (c-toggle-auto-hungry-state 1)
  (flyspell-prog-mode)
  ;(flymake-mode t)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq tab-width 4)
  (setq c-basic-offset 4))

(defun on-lisp-mode ()
  (flyspell-prog-mode)
  (setq lisp-indent-offset 2)
  (hexcolour-add-to-font-lock))

(defun on-js2-mode ()
  (hexcolour-add-to-font-lock)
  (setq js2-bounce-indent-p nil)
  (setq js2-basic-offset 2)
  (set-face-underline 'js2-warning-face nil)
  (set-face-foreground 'js2-warning-face "black")
  (set-face-background 'js2-warning-face "gold")
  (set-face-foreground 'js2-error-face "white")
  (set-face-background 'js2-error-face "red4")
)

(defun on-scss-mode()
  (setq css-indent-level 2)
  (setq css-newline-before-closing-bracket t)
  (setq css-indent-function #'cssm-c-style-indenter)
)

;Erlang Mode
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.6.5/emacs" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
