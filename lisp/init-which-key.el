(ysd-require 'which-key)
(which-key-setup-side-window-right)
(setq which-key-idle-delay 0.4
	  which-key-use-C-h-commands t
	  which-key-side-window-max-width 0.4) ;; Workaround because of error if descriptions are too long (?)

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

(provide 'init-which-key)
