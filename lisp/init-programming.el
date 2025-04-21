;; General programming settings

;; Company mode
(ysd-require 'company)
(keymap-set company-active-map "C-k" 'company-select-next-or-abort)
(keymap-set company-active-map "C-k" 'company-select-previous-or-abort)
(keymap-set company-active-map "C-k" 'company-select-next-or-abort)
(keymap-set company-active-map "C-i" 'company-select-previous-or-abort)

(setq-default tab-width 4)

;; Tree sitter
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

;; Remap major modes to their ts-mode counterparts
  (setq major-mode-remap-alist
		'((c-or-c++mode . c-or-c++-ts-mode)
		  (c-mode . c-ts-mode)
		  (c++-mode . c++-ts-mode)
		  (python-mode . python-ts-mode)))

;; Code folding using treesit-fold, a fork of ts-fold
;; MUST BE MANUALLY INSTALLED, PACKAGE IS NOT IN MELPA
(defun treesit-fold-setup ()

  (require 'treesit-fold) ;; Done without ysd-require because it is downloaded through git to site-lisp
  (ryo-modal-major-mode-keys
   'treesit-fold-mode
   ("<tab>" treesit-fold-toggle)))

(if (not (file-directory-p (locate-user-emacs-file "site-lisp/treesit-fold")))
	(message (concat "treesit-fold not installed, you should git clone it into " (locate-user-emacs-file "site-lisp/treesit-fold")))
  (treesit-fold-setup))

;; Function common to all programming modes
(defun ysd-setup-prog-modes (&optional window)
  (display-line-numbers-mode 1))

(add-hook 'prog-mode-hook 'ysd-setup-prog-modes)

(provide 'init-programming)
