(let ((keys
       '("C-@" "C-a" "C-b" "C-d" "C-e" "C-f" "C-k" "C-n" "C-p" "C-r"
         "C-s" "C-w" "C-x o")))
  (dolist (key keys)
    (global-set-key (kbd key) 'ignore)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun ysd-kill-region-or-line (&optional beg end)
  "Kill region if active, otherwise, kill whole line."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
    (kill-region beg end)
    (kill-whole-line)))

(defun ysd-copy-region-or-line (&optional beg end)
  "Copy region if active, otherwise, copy whole line."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p) ;; If there is a region
    (copy-region-as-kill beg end)
    (copy-region-as-kill (line-beginning-position)
		    (line-beginning-position 2))))

(defun ysd-delete-line (&optional line)
  "Delete the line that point is currently on."
  (interactive "d")
  (delete-region (line-beginning-position) (line-beginning-position 2)))

(defun ysd-insert-char ()
  (interactive)
  (ryo-modal-mode 0)
  (add-hook 'post-self-insert-hook 'ysd-after-insert-char))

(defun ysd-after-insert-char ()
  (ryo-modal-mode 1)
  (remove-hook 'post-self-insert-hook 'ysd-after-insert-char))

(defun bash ()
  (interactive)
  (async-shell-command "c:/windows/system32/bash.exe -i"
                       nil
                       nil))

(require 'eshell)
(defun ysd-shell ()
  "Toggle an Eshell window at the bottom of the screen."
  (interactive)
  (cl-assert eshell-buffer-name)
  (if (string= (buffer-name) eshell-buffer-name)
    (delete-window)
    (if-let ((window (get-buffer-window eshell-buffer-name))
             (default-directory (projectile-project-root)))
        (select-window window)
      (-> (get-buffer-create eshell-buffer-name)
          (display-buffer-in-side-window '(
                                           (side . bottom)
                                           (window-height . 16)))
          (select-window))
      (unless (derived-mode-p 'eshell-mode)
        (eshell-mode)))))

(require 'ryo-modal)
(require 'undo-fu)
(define-key ryo-modal-mode-map [remap self-insert-command] 'ignore)
(global-set-key (kbd "C-SPC") 'ryo-modal-mode)
(ryo-modal-keys
 ("i" previous-line)
 ("j" backward-char)
 ("k" next-line)
 ("l" forward-char)
 ("u" backward-word)
 ("o" forward-word)
 ("I" scroll-down-command)
 ("K" scroll-up-command)
 ("J" move-beginning-of-line)
 ("L" move-end-of-line)
 ("U" beginning-of-buffer)
 ("O" end-of-buffer)
 ("s" save-buffer)
 ("f" swiper-isearch)
 ("x" ysd-kill-region-or-line)
 ("c" ysd-copy-region-or-line)
 ("y" yank)
 ("X" ysd-delete-line)
 ("z" undo-fu-only-undo)
 ("Z" undo-fu-only-redo)
 ("w" ysd-insert-char)
 ("SPC" set-mark-command)
 ("b" switch-to-buffer)) ;; TODO change once I get a better way to switch buffers

;; Non modal keys
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-y") 'clipboard-yank)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-e") 'treemacs)
(global-set-key (kbd "C-t") 'ysd-shell)

(global-set-key (kbd "C-c m l") 'mc/mark-next-like-this)

(setq-default ryo-modal-cursor-type '(bar . 4))

(setq ryo-excluded-modes
      '(eshell-mode dired-mode treemacs-mode))

(define-globalized-minor-mode ryo-modal-global-mode
  ryo-modal-mode
  (lambda() (unless (or (minibufferp)
                        (member major-mode ryo-excluded-modes))
              (ryo-modal-mode 1))))
(ryo-modal-global-mode 1)

(require 'ivy)
(require 'counsel)
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)

(ivy-define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-partial-or-done) ;; Workaround because emacs equates "C-i" == "TAB"
  (ivy-define-key ivy-minibuffer-map (kbd "C-i") 'ivy-previous-line)
  (ivy-define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
  (ivy-define-key ivy-minibuffer-map (kbd "C-u") 'ivy-beginning-of-buffer)
  (ivy-define-key ivy-minibuffer-map (kbd "C-o") 'ivy-end-of-buffer)

(ivy-define-key ivy-switch-buffer-map (kbd "<tab>") 'ivy-partial-or-done) ;; "C-i" workaround
  (ivy-define-key ivy-switch-buffer-map (kbd "C-i") 'ivy-previous-line)
  (ivy-define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-next-line)
  (ivy-define-key ivy-switch-buffer-map (kbd "C-d") 'ivy-switch-buffer-kill)

(require 'treemacs)
(require 'treemacs-projectile)
(define-key treemacs-mode-map (kbd "i") 'treemacs-previous-line)
(define-key treemacs-mode-map (kbd "k") 'treemacs-next-line)
(define-key treemacs-mode-map (kbd "e") 'treemacs-quit)

(require 'projectile)
(ryo-modal-key "p" 'projectile-command-map)

(require 'company)
(define-key company-active-map (kbd "C-k") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-i") 'company-select-previous-or-abort)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'company-mode)

(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(add-hook 'c++-mode-hook 'semantic-mode)
(add-hook 'python-mode-hook 'semantic-mode)

(add-hook 'emacs-lisp-mode 'show-paren-mode)

(setq ryo-modal-default-cursor-color "white")
(require 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(load-theme 'doom-vibrant t)

(require 'org)
(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(ryo-modal-major-mode-keys
 'org-mode
 ("J" org-beginning-of-line)
 ("L" org-end-of-line))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Org Look
(add-hook 'org-mode-hook (lambda() (setq line-spacing 0.05)))
(set-fontset-font t 'unicode "Cascadia Mono" nil 'prepend)
(set-face-attribute 'org-level-1 nil :weight 'bold)
(set-face-attribute 'org-level-2 nil :weight 'bold)
(set-face-attribute 'org-level-3 nil :weight 'bold)
(set-face-attribute 'org-level-4 nil :weight 'bold)

(require 'server)
(unless (server-running-p)
  (server-start))
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

(set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(setq ring-bell-function 'ignore)
