(require 'url)
(let ((theme-path (file-name-as-directory (locate-user-emacs-file "themes"))))

  ;; Download theme file from GitHub if it does not exist
  (unless
	  (file-exists-p (concat theme-path "catppuccin-theme.el"))
    (url-copy-file "https://raw.githubusercontent.com/catppuccin/emacs/main/catppuccin-theme.el" (concat theme-path "catppuccin-theme.el")))

  ;; load theme
  (add-to-list 'custom-theme-load-path theme-path)
  (setq catppuccin-flavor 'macchiato)
  (load-theme 'catppuccin t))

(set-face-attribute 'trailing-whitespace nil :background (catppuccin-get-color 'maroon))

;; TODO: Create custom vars for default face, title face, and variable-width/mono face, and then assign them with =set-face-attribute= in init.el
;; TODO: Auto download/install(?) fonts if not installed already

(setq-default
 cursor-type '(bar . 2)
 truncate-lines t)

(setq ryo-modal-default-cursor-color (face-attribute 'cursor :background)
	  ryo-modal-cursor-type '(bar . 2)
	  ryo-modal-cursor-color (catppuccin-get-color 'text))

(dolist (mode '(org-mode-hook
				fundamental-mode-hook
				help-mode-hook))

  (add-hook mode (lambda ()
				   (setq truncate-lines nil)
				   (visual-line-mode 1))))

(provide 'init-visuals)
