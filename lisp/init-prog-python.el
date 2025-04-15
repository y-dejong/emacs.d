;; Use pyright, developed by microsoft and used for vscode

(ysd-require 'lsp-pyright)

(defun setup-python-mode ()
  (eglot-ensure)
  (company-mode 1))

(add-hook 'python-ts-mode-hook 'setup-python-mode)

(provide 'init-prog-python)
