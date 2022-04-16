(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-idle-delay 0.1)
 '(cursor-type '(bar . 4))
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(3 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(org-blank-before-new-entry '((heading . t) (plain-list-item)))
 '(org-bullets-bullet-list '(" "))
 '(org-bullets-face-name 'fixed-pitch)
 '(org-ellipsis " ▾")
 '(org-special-ctrl-a/e t)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(multiple-cursors undo-fu projectile magit company yasnippet-snippets yasnippet org doom-themes counsel ivy ryo-modal org-bullets))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Cascadia Mono"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit (org-block fixed-pitch) :extend t :foreground "#62686E" :height 0.8))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch) :height 0.8))))
 '(org-document-title ((t (:foreground "#C57BDB" :weight bold :height 2.0 :family "Source Sans Pro"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit outline-1 :family "Source Sans Pro" :weight semi-bold :extend nil :height 1.8))))
 '(org-level-2 ((t (:inherit outline-2 :family "Source Sans Pro" :weight semi-bold :extend nil :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :family "Source Sans Pro" :weight semi-bold :extend nil :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :family "Source Sans Pro" :weight semi-bold :extend nil :height 1.2))))
 '(org-meta-line ((t (:inherit fixed-pitch :foreground "#7e7e87" :height 0.8))))
 '(org-property-value ((t (:inherit shadow :foreground "#7e7e87" :height 1.0))) t)
 '(org-table ((t (:inherit fixed-pitch :foreground "#a991f1"))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:foundry "outline" :family "Merriweather")))))
