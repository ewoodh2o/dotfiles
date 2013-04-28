;;; ui.el - user interface customizations for emacs
;; Elliott Wood <elliott@two-fish.com>

;; disable menu bar, tool bar, and scroll bar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(unless (eq system-type 'darwin)
  (if (fboundp 'menu-bar-mode)   (menu-bar-mode -1)))

(which-function-mode t)            ; show current function in mode line
(show-paren-mode t)                ; highlight matching parentheses
(setq line-number-mode   t)        ; show line number in mode line
(setq column-number-mode t)        ; show column number in mode line
(setq inhibit-startup-screen t)    ; don't show splash screen
(setq initial-scratch-message nil) ; don't show scratch placeholder
(load-theme 'wombat t nil)

;; set the fringe to be less obnoxiously colored, and only on one side
(set-fringe-mode '(0 . 8))
(set-face-background 'fringe "gray14")
(set-face-foreground 'fringe "gray18")

;; set default font
(if (eq system-type 'darwin)
    (set-frame-font "Droid Sans Mono-12")
    (set-frame-font "Droid Sans Mono-10")
    (setq default-frame-alist '(
      (width . 140)
      (height . 50) )))

;; set frame title to user@host: <buffer> [modified?]
(setq frame-title-format
  (list
    (user-login-name)
    "@"
    (system-name)
    ": %b %+" ))

;; use uniqify to resolve buffer name conflicts
;; remove uniqify names after killing one of the buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)

;; customize autocomplete colors
(eval-after-load "auto-complete"
  '(lambda ()
     (set-face-attribute 'ac-candidate-face nil
       :background (face-foreground 'default)
       :foreground (face-background 'default)
       :box nil)
     (set-face-attribute 'ac-selection-face nil
       :background (face-foreground 'font-lock-keyword-face)
       :foreground "#000000"
       :bold t
       :box nil)))

;; use rainbow-mode for colors
(eval-after-load "rainbow-mode"
  '(lambda ()
     (nconc rainbow-html-colors-major-mode-list
       '(css-mode scss-mode emacs-lisp-mode javascript-mode))))


(defun scale-colour (colour factor)
  "Scale the given hex colour (#112233) by the given factor.
This used specifically to make whitespace appear as a slightly darker color
than the background of the buffer."
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


;; show leading tabs and trailing whitespace
;; as slightly darker background color
(add-hook 'font-lock-mode-hook
  (lambda ()
    (font-lock-add-keywords
      nil
      '(("^[\t ]*\t[\t ]*" 0 'trailing-whitespace prepend)))))

(set-face-background 'trailing-whitespace
  (scale-colour (face-background 'default) 0.75))
(setq-default show-trailing-whitespace t)

;; turn on line numbers
(global-linum-mode t)
(setq linum-format "%3d ")
(set-face-foreground 'linum "gray18")
(set-face-background 'linum "gray14")

;; diff mode color settings
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green3")
     (set-face-foreground 'diff-removed "red4")))

;; flycheck color settings
(eval-after-load 'flycheck
  '(progn
     (set-face-attribute 'flycheck-warning-face nil
       :background "orange4"
       :foreground "white"
       :bold nil
       :box nil
       )

     (set-face-attribute 'flycheck-error-face nil
       :background "red4"
       :foreground "white"
       :bold nil
       :box nil
       )))

(set-frame-size (selected-frame) 180 50)

