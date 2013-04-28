;; behavior.el - customization for varios emacs behavior
;; Elliott Wood <elliott@two-fish.com>

(global-auto-revert-mode t)         ; track file updates from disk
(setq kill-whole-line t)            ; include EOL when killing lines
(setq-default indent-tabs-mode nil) ; never use tabs for indenting
(setq-default tab-width 2)          ; 2 spaces per tab
(iswitchb-mode t)                   ; enhanced buffer switching
(delete-selection-mode t)           ; when region is active, delete kills region
(setq ring-bell-function 'ignore)   ; don't beep on error/end of buffer

(setq-default scroll-conservatively 1)    ; scroll one line at a time when the
                                          ; focus moves past end of buffer
(setq-default next-line-add-newlines nil) ; don't add newlines when scrolling
                                          ; past end of buffer

(defalias 'yes-or-no-p 'y-or-n-p)   ; always ask Y/N? instead of yes/no.


;; don't let the pointer go into the minibuffer prompt and get stuck
(setq minibuffer-prompt-properties
  (plist-put minibuffer-prompt-properties 'point-entered
    'minibuffer-avoid-prompt))

;; stop writing annoying autosave files to the current directory.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; iswitchb customization
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
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

(setq iswitchb-buffer-ignore
  '("^ " "^\\*[^s][^c][^r][^a][^t][^c][^h]"))


;; flycheck mode everywhere!!!!
(add-hook 'after-init-hook #'global-flycheck-mode)
