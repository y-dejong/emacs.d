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

(defvar ysd-needed-packages
  '(company
    counsel
    doom-themes
    flycheck
    ivy
    js2-mode
    magit
    multiple-cursors
    org
    org-bullets
    projectile
    ryo-modal
    tide
    treemacs
    treemacs-projectile
    undo-fu
    which-key
    yasnippet)
  "Packges that are used by init, and should be installed if not present.")

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE with MIN-VERSION.
If NO-REFRESH is nil, `package-refresh-contents' is called."
  (unless (package-installed-p package min-version)
    (unless (or (assoc package package-archive-contents) no-refresh)
      (message "Missing package: %s" package)
      (package-refresh-contents))
    (package-install package)))

(dolist (package ysd-needed-packages)
  (require-package package))

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

(defun ysd-yank ()
  "Yank or cycle through the kill ring."
  (interactive "*")
  (if (eq last-command 'yank)
    (yank-pop)
    (setq kill-ring-yank-pointer kill-ring)
    (yank)))

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

(defun ysd-swiper-isearch (&optional beg end)
  "swiper-isearch using the current region if non-nil."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (not (use-region-p))
    (swiper-isearch)
    (deactivate-mark)
    (swiper-isearch (buffer-substring beg end))))

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
 ("f" ysd-swiper-isearch)
 ("r" query-replace)
 ("x" ysd-kill-region-or-line)
 ("c" ysd-copy-region-or-line)
 ("y" ysd-yank)
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

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

(require 'eaf)
(require 'eaf-browser)
(require 'eaf-demo)
(require 'eaf-terminal)

(require 'ivy)
(require 'counsel)
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(ivy-define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-partial-or-done) ;; Workaround because emacs equates "C-i" == "TAB"
  (ivy-define-key ivy-minibuffer-map (kbd "C-i") 'ivy-previous-line)
  (ivy-define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
  (ivy-define-key ivy-minibuffer-map (kbd "C-u") 'ivy-beginning-of-buffer)
  (ivy-define-key ivy-minibuffer-map (kbd "C-o") 'ivy-end-of-buffer)

(ivy-define-key ivy-switch-buffer-map (kbd "<tab>") 'ivy-partial-or-done) ;; "C-i" workaround
  (ivy-define-key ivy-switch-buffer-map (kbd "C-i") 'ivy-previous-line)
  (ivy-define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-next-line)
  (ivy-define-key ivy-switch-buffer-map (kbd "C-d") 'ivy-switch-buffer-kill)

(when (display-graphic-p)
  (require 'all-the-icons))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(require 'treemacs)
(require 'treemacs-projectile)
(define-key treemacs-mode-map (kbd "i") 'treemacs-previous-line)
(define-key treemacs-mode-map (kbd "k") 'treemacs-next-line)
(define-key treemacs-mode-map (kbd "e") 'treemacs-quit)

(defun ysd-make-projects-list ()
  (when (file-exists-p treemacs-persist-file)
    (with-temp-buffer
      (let (linkspecs)
        (insert-file-contents treemacs-persist-file)
        (while (not (or (eq (line-end-position) (point-max))
                        (eq (line-beginning-position 2) (point-max))))
          (re-search-forward "^\\*\\*\s" nil 1)
          (push (buffer-substring (point) (line-end-position)) linkspecs)
          (re-search-forward "^\s-\spath\s::\s" nil t)
          (push (buffer-substring (point) (line-end-position)) linkspecs))
        (reverse linkspecs)))))

(defun ysd-startup-screen ()
  "Display a startup screen with list of projects from treemacs."
  (let ((splash-buffer (get-buffer-create "*Yasper Emacs*")))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t)
            (default-text-properties '(face variable-pitch))
            (projects (ysd-make-projects-list)))
        (erase-buffer)
        (setq default-directory command-line-default-directory)
        (insert "Welcome to Yasper's Emacs.\n\n")
        (insert "Agenda:\n")
        (insert-button "View Full Agenda"
                       'face 'link
                       'action `(lambda (_button) (find-file (concat user-emacs-directory "todo.org")))
                       'help-echo (concat "mouse-2, RET: " (concat user-emacs-directory "todo.org"))
                       'follow-link t)

        (insert "\n\nHack Init: ")
        (insert-button "init.org"
                       'face 'link
                       'action `(lambda (_button) (find-file (concat user-emacs-directory "init.org")))
                       'help-echo (concat "mouse-2, RET: " (concat user-emacs-directory "init.org"))
                       'follow-link t)
        (insert "\n\nOpen Project:\n")
        (while projects
          (insert-button (pop projects)
                         'face 'link
                         'action `(lambda (_button) (dired ,(car projects)))
                         'help-echo (concat "mouse-2, RET: " (pop projects))
                         'follow-link t)
          (insert "\n")))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (beginning-of-buffer))
    splash-buffer))

(require 'projectile)
(ryo-modal-key "p" 'projectile-command-map)

(require 'company)
(define-key company-active-map (kbd "C-k") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-i") 'company-select-previous-or-abort)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'company-mode)

(require 'semantic)
;;(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(add-hook 'c++-mode-hook 'semantic-mode)
(add-hook 'python-mode-hook 'semantic-mode)

(add-hook 'emacs-lisp-mode 'show-paren-mode)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (company-mode 1))

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

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

(define-key org-mode-map (kbd "C-<tab>") nil)

(require 'server)
(unless (server-running-p)
  (server-start))
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

(set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(setq-default
 ring-bell-function 'ignore
 company-idle-delay 0.1
 cursor-type '(bar . 4)
 initial-buffer-choice 'ysd-startup-screen
 line-number-mode t
 mouse-wheel-progressive-speed nil
 org-blank-before-new-entry '((heading . t) (plain-list-item))
 org-bullets-bullet-list '(" ")
 org-bullets-face-name 'fixed-pitch
 org-ellipsis " ▾"
 org-special-ctrl-a/e t
 show-paren-mode t
 truncate-lines t
 which-key-mode t
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
