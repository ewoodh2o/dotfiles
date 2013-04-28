;;; modes.el --- hooks for customizing various modes
;; Elliott Wood <elliott@two-fish.com>

(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
     (setq lisp-indent-offset 2)
     (rainbow-mode t)))

(add-hook 'scss-mode-hook
  '(lambda ()
     (setq scss-compile-at-save nil)
     (setq css-indent-level 2)
     (auto-complete-mode t)
;     (setq css-newline-before-closing-bracket t)
;     (setq css-indent-function #'cssm-c-style-indenter)
     (rainbow-mode t)))

(add-hook 'css-mode-hook
  '(lambda ()
     (setq css-indent-level 2)
     (rainbow-mode t)))

(add-hook 'rhtml-mode-hook
  '(lambda ()
     (rainbow-mode t)
     (auto-complete-mode t)))

(add-hook 'sh-mode-hook
  '(lambda ()
     (setq sh-basic-offset 2)))

(add-hook 'coffee-mode-hook
  '(lambda ()
     (auto-complete-mode t)
     (make-local-variable 'tab-width)
     (set 'tab-width 2)))

(add-hook 'go-mode-hook
  '(lambda ()
     (auto-complete-mode t)
     (setq indent-tabs-mode nil)))
