;; Project management with projectile and treemacs

(ysd-require 'projectile)
(ysd-require 'counsel-projectile)
(define-key projectile-mode-map (kbd "C-p") projectile-command-map)
(setq projectile-switch-project-action #'projectile-dired)
(projectile-mode 1)
(counsel-projectile-mode 1)

(provide 'init-projects)
