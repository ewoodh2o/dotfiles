;; ruby.el - ruby mode customization for emacs
;; Elliott Wood <elliott@two-fish.com>

;; fix ruby-mode multi-line parameter indentation
(setq-default ruby-deep-indent-paren       nil)
(setq-default ruby-deep-indent-paren-style nil)
(setq-default ruby-deep-arglist            nil)

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

;; add additional filenames to ruby-mode
(nconc auto-mode-alist
  (list
    '("Gemfile$"    . ruby-mode)
    '("Rakefile$"   . ruby-mode)
    '("\\.gemspec$" . ruby-mode)
    '("\\.ru"       . ruby-mode)
    '("\\.rake"     . ruby-mode)))


(defun align-ruby-hash (BEG END)
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\) =>" 1 1))

(add-hook 'ruby-mode-hook
  '(lambda ()
     (local-set-key (kbd "s-=") 'align-ruby-hash)
     ;; (ruby-electric-mode t)
     ;; (electric-pair-mode t)
     (rainbow-mode t)))
