;; Use tide as a typescript server
;; TODO: replace with Go Typescript LSP when available

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

(provide 'init-prog-typescript)
