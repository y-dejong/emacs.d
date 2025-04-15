;; Use magit

(ysd-require 'magit)

(setq magit-auto-revert-mode 0)
(keymap-set magit-status-mode-map (kbd "i") 'magit-previous-line)
(keymap-set magit-status-mode-map (kbd "k") 'magit-next-line)
(keymap-set magit-status-mode-map (kbd "I") 'magit-gitignore)
(keymap-set magit-status-mode-map (kbd "K") 'magit-discard)
(keymap-unset magit-status-mode-map "C-<tab>")
(push 'magit-status-mode ryo-excluded-modes)

(provide 'init-git)
