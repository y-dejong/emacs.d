(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)

;; Load environment variables from env.el
(setq env-file (locate-user-emacs-file "env.el"))
(if (file-exists-p env-file) (load env-file))

(require 'init-utils)
(require 'init-modal)
(require 'init-which-key)

(require 'init-completion)

(require 'init-visuals)
(require 'init-mode-line)

(require 'init-projects)
(require 'init-files)

(require 'init-dashboard)

(require 'init-programming)
(require 'init-prog-typescript)
(require 'init-prog-c)
(require 'init-prog-rust)
(require 'init-prog-python)

(require 'init-terminal)
(require 'init-git)

(require 'init-ai)

;; Miscellaneous
(setq-default
 cursor-type '(bar . 2)
 line-number-mode t
 column-number-mode t
 mouse-wheel-progressive-speed nil
 pixel-scroll-precision-mode t
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
(put 'upcase-region 'disabled nil)
