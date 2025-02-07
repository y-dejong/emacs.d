(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun ysd-require (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE with MIN-VERSION.
If NO-REFRESH is nil, `package-refresh-contents' is called."
  (unless (package-installed-p package min-version)
	(message "Missing package: %s" package)
	(unless (or (assoc package package-archive-contents) no-refresh)
  (package-refresh-contents))
	(package-install package)
	(push package package-selected-packages))
  (require package))

;; Add site-lisp for manually installed packages
(let ((default-directory (locate-user-emacs-file "site-lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(setq env-file (locate-user-emacs-file "env.el"))
(if (file-exists-p env-file) (load env-file))

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

(defmacro define-key-with-fallback (keymap key def condition &optional mode)
  "Define key with fallback. Binds KEY to definition DEF in keymap KEYMAP, 
   the binding is active when the CONDITION is true. Otherwise turns MODE off 
   and re-enables previous definition for KEY. If MODE is nil, tries to recover 
   it by stripping off \"-map\" from KEYMAP name."
  `(define-key ,keymap ,key
	 (lambda () (interactive)
		(if ,condition ,def
		  (let* ((,(if mode mode
					 (let* ((keymap-str (symbol-name keymap))
							(mode-name-end (- (string-width keymap-str) 4)))
					   (if (string= "-map" (substring keymap-str mode-name-end))
						   (intern (substring keymap-str 0 mode-name-end))
						 (error "Could not deduce mode name from keymap name (\"-map\" missing?)")))) 
				  nil)
				 (original-func (key-binding ,key)))
			(call-interactively original-func))))))

(ysd-require 'ryo-modal)

(define-key ryo-modal-mode-map [remap self-insert-command] 'ignore) ;; Make all letters/etc. do nothing

(global-set-key (kbd "<escape>") 'ryo-modal-mode)

(ryo-modal-keys
 ("i" previous-line)
 ("k" next-line)
 ("j" backward-char)
 ("l" forward-char)
 ("u" backward-word)
 ("o" forward-word)
 ("I" scroll-down-line)
 ("K" scroll-up-line)
 ("J" move-beginning-of-line)
 ("L" move-end-of-line)
 ("U" beginning-of-buffer)
 ("O" end-of-buffer)
 ("r" query-replace)
 ("x" ysd-kill-region-or-line)
 ("c" ysd-copy-region-or-line)
 ("y" ysd-yank)
 ("z" undo)
 ("Z" undo-redo)
 ("g" goto-map)
 (";" comment-line)
 ("SPC" set-mark-command))

(global-set-key (kbd "C-<tab>") 'other-window) ;; TODO Adapt for terminal interface

(setq ryo-excluded-modes
  '(eshell-mode dired-mode treemacs-mode vterm-mode inferior-python-mode))

(add-hook 'window-selection-change-functions
	  (lambda (buf) (interactive)
		(unless (or (minibufferp (window-buffer (old-selected-window)))
			(minibufferp (current-buffer))
			(member major-mode ryo-excluded-modes))
	  (ryo-modal-mode 1))))


(define-globalized-minor-mode ryo-modal-global-mode
  ryo-modal-mode
  (lambda () (unless (or (minibufferp)
			 (member major-mode ryo-excluded-modes))
	   (ryo-modal-mode 1))))

(ryo-modal-global-mode 1)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") 'revert-buffer)

(ysd-require 'which-key)
(which-key-setup-side-window-right)
(setq which-key-idle-delay 0.4
	  which-key-use-C-h-commands t)

(defun ysd-which-key-show-top-level-excluding-ryo ()
  (interactive)
  (let ((ryo-modal-mode nil))
	(which-key-show-top-level)))
(ryo-modal-key "?" 'ysd-which-key-show-top-level-excluding-ryo)

; Workaround for a paging key that has another keybind attached in underlying modes
(defun ysd-which-key-maybe-C-h-dispatch ()
  (interactive)
  (if (which-key--popup-showing-p) (which-key-C-h-dispatch)
	(let ((which-key-mode nil))
	  (command-execute (key-binding "?")))))
(push 'ysd-which-key-maybe-C-h-dispatch which-key--paging-functions)
(keymap-set which-key-mode-map "?" 'ysd-which-key-maybe-C-h-dispatch)

; Paging keys that match the rest of the config
(keymap-set which-key-C-h-map "k" 'which-key-show-next-page-cycle)
(keymap-set which-key-C-h-map "i" 'which-key-show-previous-page-cycle)
(which-key-mode 1)

(ysd-require 'counsel)
(ivy-mode 1)

(global-set-key (kbd "C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-b") 'ivy-switch-buffer)

;; Minibuffer bindings
(ivy-define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-partial-or-done) ;; Workaround since C-i and TAB are the same, but <tab> is different
(ivy-define-key ivy-minibuffer-map (kbd "C-i") 'ivy-previous-line)
(ivy-define-key ivy-minibuffer-map (kbd "C-k") 'ivy-next-line)
(ivy-define-key ivy-minibuffer-map (kbd "C-u") 'ivy-beginning-of-buffer)
(ivy-define-key ivy-minibuffer-map (kbd "C-o") 'ivy-end-of-buffer)

;; Switch buffer bindings
(ivy-define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-next-line)
(ivy-define-key ivy-switch-buffer-map (kbd "C-d") 'ivy-switch-buffer-kill)

(defun ysd-ivy-minibuffer-grow ()
  (interactive)
  (setq-local max-mini-window-height
			  (cl-incf ivy-height)))

(defun ysd-ivy-minibuffer-shrink ()
  (interactive)
  (when (> ivy-height 2)
  (setq-local max-mini-window-height
			  (cl-decf ivy-height))
  (window-resize nil -1)))

(ivy-define-key ivy-minibuffer-map (kbd "M-I")
		'ysd-ivy-minibuffer-grow)
(ivy-define-key ivy-minibuffer-map (kbd "M-K")
		'ysd-ivy-minibuffer-shrink)

(define-key swiper-map (kbd "C-r") 'swiper-query-replace)

(defun ysd-swiper-dwim (&optional beg end)
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
	  (swiper (buffer-substring beg end))
	(call-interactively 'swiper)))
(ryo-modal-key "s" 'ysd-swiper-dwim)
(ryo-modal-key "S" 'swiper-thing-at-point)

(add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))

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

(setq-default
 cursor-type '(bar . 2)
 truncate-lines t)

(setq ryo-modal-default-cursor-color (face-attribute 'cursor :background)
  ryo-modal-cursor-type '(bar . 2)
  ryo-modal-cursor-color (catppuccin-get-color 'text))

(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
		fundamental-mode-hook
		help-mode-hook))

  (add-hook mode (lambda ()
		   (display-line-numbers-mode 0)
		   (setq truncate-lines nil)
		   (visual-line-mode 1))))

(ysd-require 'telephone-line)

(defun ysd-make-header-line-mouse-map (mouse function)
  (let ((map (make-sparse-keymap)))
	(define-key map (vector 'header-line mouse) function)
	map))

;;(set-face-attribute 'mode-line nil :background (catppuccin-get-color 'overlay1))
(set-face-attribute 'telephone-line-evil-normal nil :foreground (catppuccin-get-color 'red) :background (catppuccin-get-color 'base))
(set-face-attribute 'telephone-line-evil-insert nil :foreground (catppuccin-get-color 'green) :background (catppuccin-get-color 'base))

(defface ysd-tele-line-modified
  `((t (:foreground ,(catppuccin-get-color 'red) :background ,(catppuccin-get-color 'surface2))))
  "Telephone line modified face"
  :group 'telephone-line)

(defface ysd-surface2-bg
  `((t (:background ,(catppuccin-get-color 'surface2))))
  "Surface2 background face"
  :group 'telephone-line)

(defface ysd-surface1-bg
  `((t :background ,(catppuccin-get-color 'surface1)))
  "Surface1 background face"
  :group 'telephone-line)

(defface ysd-invisible
  `((t (:foreground ,(catppuccin-get-color 'base) :background ,(catppuccin-get-color 'base))))
  "Surface1 background face"
  :group 'telephone-line)

(defun ysd-tele-line-surface1-face (active)
  (cond ((not active) 'mode-line-inactive)
		(t 'ysd-surface1-bg)))

(defun ysd-tele-line-surface2-face (active)
  (cond ((not active) 'mode-line-inactive)
		(t 'ysd-surface2-bg)))


(defun ysd-tele-line-buffer-face (active)
  'ysd-invisible)

(telephone-line-defsegment* ysd-buffer-segment ()
  " ")

(push '(surface2 . ysd-tele-line-surface2-face) telephone-line-faces)
(push '(surface1 . ysd-tele-line-surface1-face) telephone-line-faces)
(push '(buffer . ysd-tele-line-buffer-face) telephone-line-faces)

(defun ysd-tele-line-modified-face (active)
  (cond ((not active) 'mode-line-inactive)
		((buffer-modified-p) 'ysd-tele-line-modified)
		(t 'ysd-surface2-bg)))
(push '(modif . ysd-tele-line-modified-face) telephone-line-faces)

(defun ysd-modal-face (active)
  "Return an appropriate face depending whether ryo-modal is activated, given whether frame is ACTIVE."
  (cond ((not active) 'ysd-invisible)
		((not (boundp 'ryo-modal-mode)) 'mode-line)
		((not ryo-modal-mode) 'telephone-line-evil-insert)
		(t 'telephone-line-evil-normal)))
(push '(ysd-modal . ysd-modal-face) telephone-line-faces)

(telephone-line-defsegment* ysd-ryo-modal-segment ()
  "◉")

(defun ysd-set-coding-system (e)
  (interactive "e")
  (with-selected-window (posn-window (event-start e))
	(call-interactively 'set-buffer-file-coding-system)))

(telephone-line-defsegment* ysd-telephone-line-encoding-segment ()
  (propertize
   (upcase (symbol-name
			(plist-get (coding-system-plist buffer-file-coding-system) :name)))
   'help-echo "Buffer coding system:\nmouse-1: Change"
   'local-map (ysd-make-header-line-mouse-map
			   'mouse-1 (lambda (e)
						  (interactive "e")
						  (with-selected-window (posn-window (event-start e))
							(call-interactively 'set-buffer-file-coding-system))))
   'mouse-face 'mode-line-highlight))


(telephone-line-defsegment* ysd-telephone-line-eol-segment ()
  (propertize
   (pcase (coding-system-eol-type buffer-file-coding-system)
	 (0 "LF")
	 (1 "CRLF")
	 (2 "CR"))
   'help-echo "End-of-line style:\nmouse-1: Cycle"
   'local-map (ysd-make-header-line-mouse-map
			   'mouse-1 'mode-line-change-eol)
   'mouse-face 'mode-line-highlight))

;; Circle separator
(defvar telephone-line-halfcircle-right
  (make-instance 'telephone-line-separator
				 :axis-func (lambda (x) (let ((result (sqrt (- 9.869 (expt x 2)))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-right))

(defvar telephone-line-halfcircle-left
  (make-instance 'telephone-line-separator
				 :axis-func (lambda (x) (let ((result (- (sqrt (- 9.869 (expt x 2))))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-left))

(defvar telephone-line-halfcircle-hollow-right
  (make-instance 'telephone-line-subseparator
				 :axis-func (lambda (x) (let ((result (sqrt (- 9.869 (expt x 2)))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-hollow-right))

(defvar telephone-line-halfcircle-hollow-left
  (make-instance 'telephone-line-subseparator
				 :axis-func (lambda (x) (let ((result (- (sqrt (- 9.869 (expt x 2))))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-hollow-left))

(setq
 telephone-line-lhs
 '((ysd-modal . (ysd-ryo-modal-segment))
   (modif . (telephone-line-buffer-name-segment))
   (surface1 . (telephone-line-major-mode-segment
				telephone-line-minor-mode-segment)))
 telephone-line-rhs
 '((surface1 . (ysd-telephone-line-encoding-segment))
   (surface2 . (ysd-telephone-line-eol-segment))
   (buffer . (ysd-buffer-segment)))

 telephone-line-target 'header-line ;; TODO disable header-line option in Emacs <28
 telephone-line-primary-left-separator telephone-line-halfcircle-left
 telephone-line-primary-right-separator telephone-line-halfcircle-right)

(setq-default mode-line-format nil)
(telephone-line-mode 1)

(defun swap-header-and-mode-line (symbol newval operation where)
  (with-current-buffer where
	(when (and (eq operation 'set) (not (eq newval (default-value 'header-line-format))))
	  (setq mode-line-format newval)
	  (run-with-timer 0 nil (lambda () (setq header-line-format (default-value 'header-line-format)))))))
;; run-with-timer 0 waits until after function ends to change header-line-format back to the original value

(add-variable-watcher 'header-line-format 'swap-header-and-mode-line)

(ysd-require 'diminish)
(let ((diminished-modes
	   '(ivy-mode ryo-modal-mode which-key-mode)))
  (dolist (mode diminished-modes)
	(diminish mode)))

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

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(keymap-set dired-mode-map "i" 'dired-previous-line)
(keymap-set dired-mode-map "k" 'dired-next-line)
(keymap-set dired-mode-map "?" 'which-key-show-top-level)
(keymap-set dired-mode-map "m" 'dired-do-rename)
(keymap-set dired-mode-map "c" 'dired-do-copy)

(defun get-random-banner ()
  "Get random banner from startup-banners"
  (let ((startup-banners-dir (locate-user-emacs-file "startup-banners")))
	(if (file-directory-p startup-banners-dir)
		(let ((files (cl-remove-if-not (lambda (file) (string= (file-name-extension file) "txt"))
										   (directory-files startup-banners-dir))))
		  (concat (file-name-as-directory startup-banners-dir) (nth (random (length files)) files)))
	  (message "Startup banners dir not found")
	  'logo)))

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
		dashboard-startup-banner (get-random-banner)
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

(add-hook 'dashboard-before-initialize-hook
		  (lambda()
			(setq dashboard-startup-banner (get-random-banner))))

(add-hook 'dashboard-mode-hook
		  (lambda () (setq-local show-trailing-whitespace nil))) ;; Ruins ASCII art


(ysd-init-dashboard)

(ysd-require 'company)
(keymap-set company-active-map "C-k" 'company-select-next-or-abort)
(keymap-set company-active-map "C-k" 'company-select-previous-or-abort)
(keymap-set company-active-map "C-k" 'company-select-next-or-abort)
(keymap-set company-active-map "C-i" 'company-select-previous-or-abort)

(setq-default
 tab-width 4)

(require 'url)
(let ((tree-sitter-dir (file-name-as-directory (locate-user-emacs-file "tree-sitter"))))
  (unless (file-exists-p tree-sitter-dir)
	(make-directory tree-sitter-dir)
	(url-copy-file
	 "https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/download/0.12.224/tree-sitter-grammars.x86_64-unknown-linux-gnu.v0.12.224.tar.gz"
	 (concat tree-sitter-dir "tree-sitter-grammars.tar.gz"))
	(shell-command (concat "tar xzf " (concat tree-sitter-dir "tree-sitter-grammars.tar.gz") " -C " tree-sitter-dir))
;; Rename *.so to libtree-sitter-*.so
(dolist (file (directory-files tree-sitter-dir t "\\.so$"))
  (rename-file file (concat tree-sitter-dir "libtree-sitter-" (file-name-nondirectory file))))))

(setq major-mode-remap-alist
	  '((c-or-c++mode . c-or-c++-ts-mode)
		(c-mode . c-ts-mode)
		(c++-mode . c++-ts-mode)
		(python-mode . python-ts-mode)))

(defun treesit-fold-setup ()

  (require 'treesit-fold) ;; Done without ysd-require because it is downloaded through git to site-lisp
  (ryo-modal-major-mode-keys
   'treesit-fold-mode
   ("<tab>" treesit-fold-toggle)))

(if (not (file-directory-p (locate-user-emacs-file "site-lisp/treesit-fold")))
	(message (concat "treesit-fold not installed, you should git clone it into " (locate-user-emacs-file "site-lisp/treesit-fold")))
  (treesit-fold-setup))

(ysd-require 'typescript-mode)
(ysd-require 'tide)
(ysd-require 'flycheck)
(ysd-require 'company)
(ysd-require 'prettier-js)

(defun setup-typescript-mode ()
  (interactive)

  ;;Tide setup
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (company-mode 1)
  (electric-pair-mode 1)
  (origami-mode 1)
  (indent-tabs-mode -1)

  (setq
   typescript-indent-level tab-width
   js-indent-level 2))

(add-hook 'typescript-mode-hook #'setup-typescript-mode)
;;(add-hook 'typescript-mode-hook 'prettier-js-mode)

(add-hook 'js-mode-hook #'setup-typescript-mode)
;;(add-hook 'js-mode-hook 'prettier-js-mode)

(ysd-require 'flycheck)
(ysd-require 'eglot)

(setq-default
 c-ts-indent-offset tab-width
 c-indentation-style "linux") ;; THis seems to be overwritten when the mode loads

(push (cons '(c-mode c-ts-mode c++-mode c++-ts-mode) ;; Use ccls over clangd
			(eglot-alternatives
			 '("ccls" "clangd")))
	  eglot-server-programs)

(defun setup-c++-mode ()
  (interactive)
  (setq
   c-basic-offset tab-width
   c-ts-indent-offset tab-width
   c-indentation-style "linux")
  (treesit-fold-mode 1)
  (electric-pair-mode 1)
  (company-mode 1)
  (eglot-ensure))

(add-hook 'c++-ts-mode-hook 'setup-c++-mode)
(add-hook 'c-ts-mode-hook 'setup-c++-mode)
(add-hook 'c++-mode-hook 'setup-c++-mode)
(add-hook 'c-mode-hook 'setup-c++-mode)

(ysd-require 'vterm)
(define-key vterm-mode-map (kbd "<escape>") nil)
(define-key vterm-mode-map (kbd "C-c <escape>") 'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-b") nil)
(define-key vterm-mode-map (kbd "C-c C-b") 'vterm--self-insert)

(ysd-require 'magit)

(keymap-set magit-status-mode-map (kbd "i") 'magit-previous-line)
(keymap-set magit-status-mode-map (kbd "k") 'magit-next-line)
(keymap-set magit-status-mode-map (kbd "I") 'magit-gitignore)
(keymap-set magit-status-mode-map (kbd "K") 'magit-discard)
(keymap-unset magit-status-mode-map "C-<tab>")
(push 'magit-status-mode ryo-excluded-modes)

(setq org-fold-core-style 'overlays) ;; Workaround to folding sometimes being broken

;; Set up llm package
(ysd-require 'llm)
(require 'llm-openai)
(setq llm-warn-on-nonfree nil
	  llm-provider-groq (make-llm-openai-compatible
						 :url "https://api.groq.com/openai/v1"
						 :key groq-api-key
						 :chat-model "llama-3.3-70b-versatile"))

(defun remove-first-and-last-line (str)
  "Remove the first and last lines of the given string STR."
  (let ((lines (split-string str "\n" t)))
	(when ( > (length lines) 2)
	  (mapconcat 'identity (cdr (butlast lines)) "\n"))))

(defun ysd-llm-generate-code ()
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
		(end (if (use-region-p) (region-end) (line-end-position)))
		(prompt (string-trim (buffer-substring start end))))
	(if (string= prompt "")
		(setq prompt (read-string "Enter Prompt: "))
	  (delete-region start end))
	(insert (llm-chat
			 llm-provider-groq
			 (llm-make-chat-prompt prompt :context "Respond with only code")))))
(ryo-modal-key "!" 'ysd-llm-generate-code)

;; Set default variables
(setq-default
 cursor-type '(bar . 2)
 line-number-mode t
 column-number-mode t
 mouse-wheel-progressive-speed nil
 truncate-lines t
 show-trailing-whitespace t
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 ring-bell-function 'ignore)

;; Set global minor modes
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)

;; Set fullscreen
;; TODO: make fullscreen startup a 'customize' option
(add-to-list 'default-frame-alist '(internal-border-width . 24))
(add-to-list 'default-frame-alist '(alpha-background . 70))

(setq custom-file (locate-user-emacs-file "custom.el"))
(if (file-exists-p custom-file) (load custom-file))
