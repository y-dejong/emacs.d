;; File management with dired

(require 'dired)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(keymap-set dired-mode-map "i" 'dired-previous-line)
(keymap-set dired-mode-map "k" 'dired-next-line)
(keymap-set dired-mode-map "?" 'which-key-show-top-level)
(keymap-set dired-mode-map "m" 'dired-do-rename)
(keymap-set dired-mode-map "c" 'dired-do-copy)

(provide 'init-files)
