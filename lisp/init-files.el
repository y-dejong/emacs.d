;; File management with dired

(require 'dired)
(push 'dired-mode ryo-excluded-modes)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(keymap-set dired-mode-map "i" 'dired-previous-line)
(keymap-set dired-mode-map "k" 'dired-next-line)
(keymap-set dired-mode-map "u" 'dired-up-directory)
(keymap-set dired-mode-map "?" 'dirvish-dispatch)
(keymap-set dired-mode-map "<tab>" 'dirvish-subtree-toggle)
(keymap-set dired-mode-map "m" 'dired-do-rename)
(keymap-set dired-mode-map "c" 'dired-do-copy)

(ysd-require 'dirvish) ;; Overhaul dired
(add-hook 'dired-mode-hook 'dired-omit-mode) ;; Omits . and ..
(dirvish-override-dired-mode 1)
(setq dirvish-attributes '(subtree-state vc-state collapse all-the-icons file-time file-size)
	  dired-mouse-drag-files t
	  mouse-drag-and-drop-region-cross-program t)

;; Dirvish in side window
(setq dirvish-side-follow-mode t
	  dirvish-side-attributes '(subtree-state vc-state all-the-icons))

(global-set-key (kbd "C-e") 'dirvish-side)

(provide 'init-files)
