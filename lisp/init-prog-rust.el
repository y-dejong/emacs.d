(ysd-require 'rustic)
(setq rustic-lsp-client 'eglot
	  rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
(defun ysd-setup-rustic ()
  (flymake-mode -1))

(add-hook 'rustic-mode 'ysd-setup-rustic)

(provide 'init-prog-rust)
