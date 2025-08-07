;; Use vterm, best emulation and support of complex TUIs

(ysd-require 'vterm)
(define-key vterm-mode-map (kbd "<escape>") nil)
(define-key vterm-mode-map (kbd "C-c <escape>") 'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-b") nil)
(define-key vterm-mode-map (kbd "C-c C-b") 'vterm--self-insert)

(keymap-global-set "C-t" 'vterm-other-window)

(provide 'init-terminal)
