;; Auto generate ASCII art and use dashboard.el

;; AI generated
  (defun get-random-banner ()
	"Generate a random banner using figlet with a random font from marked_fonts.txt"
	(let* ((startup-banners-dir (locate-user-emacs-file "startup-banners"))
		   (figlet-path (concat (file-name-as-directory startup-banners-dir) "figlet"))
		   (fonts-dir (concat (file-name-as-directory startup-banners-dir) "fonts/"))
		   (fonts-file (concat (file-name-as-directory startup-banners-dir) "marked_fonts.txt"))
		   (fonts-list (with-temp-buffer
						 (insert-file-contents fonts-file)
						 (split-string (buffer-string) "\n" t)))
		   (random-font (nth (random (length fonts-list)) fonts-list))
		   (banner-text (shell-command-to-string (format "%s -d %s -f \"%s\" \"Emacs\"" figlet-path fonts-dir random-font))))
	  banner-text))

(when (display-graphic-p)
	(ysd-require 'all-the-icons))

  (setq dashboard-set-heading-icons t) ;; Workaround, icons won't load unless this is set before the require
  (ysd-require 'dashboard)

  ;; Download a cooler emacs logo
  (setq dashboard-logo-file (locate-user-emacs-file "gnu_color.svg"))
  (unless (file-exists-p dashboard-logo-file)
	(url-copy-file "https://raw.githubusercontent.com/egstatsml/emacs_fancy_logos/refs/heads/main/gnu_color.svg" dashboard-logo-file))

  (defun ysd-init-dashboard ()

	;; Keybinds
	(push 'dashboard-mode ryo-excluded-modes)
	(keymap-set dashboard-mode-map (kbd "i") 'dashboard-previous-line)
	(keymap-set dashboard-mode-map (kbd "k") 'dashboard-next-line)
	(keymap-set dashboard-mode-map (kbd "g") 'dashboard-refresh-buffer)

	(setq dashboard-projects-backend 'projectile
		  initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)) ;; Shows dashboard even if launched with emacsclient instead of emacs
		  dashboard-banner-logo-title "Yasper's Emacs"
		  dashboard-startup-banner (make-temp-file "emacs-banner" nil ".txt" (get-random-banner))
		  dashboard-center-content t
		  dashboard-set-file-icons t
		  dashboard-projects-show-base t
		  dashboard-projects-item-format "%s"
		  dashboard-icon-type 'all-the-icons
		  dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
		  dashboard-items '((projects . 10)
							(bookmarks . 5)
							(recents . 5)))
	(dashboard-setup-startup-hook))

  ;; (add-hook 'dashboard-before-initialize-hook
  ;; 		  (lambda()
  ;; 			(setq dashboard-startup-banner (get-random-banner))))

  (add-hook 'dashboard-mode-hook
			(lambda () (setq-local show-trailing-whitespace nil))) ;; Ruins ASCII art


  (ysd-init-dashboard)

(provide 'init-dashboard)
