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

;; TODO: Switch back to flymake because eglot?

(provide 'init-prog-c)
