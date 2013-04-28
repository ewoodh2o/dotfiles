;; keybinds.el - global key rebinds for emacs
;; Elliott Wood <elliott.wood@two-fish.com>

(global-set-key [M-g]         'goto-line)
(global-set-key [M-up]        'move-text-up)
(global-set-key [M-down]      'move-text-down)
(global-set-key [C-kp-delete] 'kill-word)

;; (global-set-key (kbd "s-/") 'comment-region)
;; (global-set-key (kbd "s-?") 'uncomment-region)
;; (global-set-key (kbd "s-[") 'ew-outdent)
;; (global-set-key (kbd "s-]") 'ew-indent)
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key (kbd "RET") 'newline-and-indent)
