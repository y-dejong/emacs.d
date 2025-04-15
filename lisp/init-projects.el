;; Project management with projectile and treemacs

(ysd-require 'projectile)
(ysd-require 'counsel-projectile)
(define-key projectile-mode-map (kbd "C-p") projectile-command-map)
(setq projectile-switch-project-action #'projectile-dired)
(projectile-mode 1)
(counsel-projectile-mode 1)

(ysd-require 'treemacs)
(ysd-require 'treemacs-projectile)
(define-key treemacs-mode-map (kbd "i") 'treemacs-previous-line)
(define-key treemacs-mode-map (kbd "k") 'treemacs-next-line)

(global-set-key (kbd "C-e") 'treemacs)

(provide 'init-projects)
