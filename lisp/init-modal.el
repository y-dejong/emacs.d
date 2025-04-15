;; Custom modal setup based around ergonomics, using ryo-modal

(ysd-require 'ryo-modal)

(define-key ryo-modal-mode-map [remap self-insert-command] 'ignore) ;; Make all letters/etc. do nothing

(global-set-key (kbd "<escape>") 'ryo-modal-mode)

(ryo-modal-keys
 ("i" previous-line)
 ("k" next-line)
 ("j" backward-char)
 ("l" forward-char)
 ("u" backward-same-syntax)
 ("o" forward-same-syntax)
 ("U" beginning-of-defun)
 ("O" end-of-defun)
 ("I" scroll-down-line)
 ("K" scroll-up-line)
 ("J" beginning-or-indentation)
 ("L" move-end-of-line)
 ("<" beginning-of-buffer)
 (">" end-of-buffer)
 ("n" insert-line-below)
 ("w" insert-one-char)
 ("W" replace-one-char)
 ("r" query-replace)
 ("x" ysd-kill-region-or-line)
 ("c" ysd-copy-region-or-line)
 ("y" ysd-yank)
 ("z" undo)
 ("Z" undo-redo)
 ("g"
  (("g" goto-line)
   ("e"
	(("p" previous-error)
	 ("n" next-error)))
   ("w" avy-goto-word-1)
   ("m" jump-to-paren-match)))
 ("b"
  (("b" point-to-register)
   ("j" jump-to-register)
   ("l" counsel-register)))
 ("/" comment-dwim)
 ("SPC" set-mark-command))

(global-set-key (kbd "C-<tab>") 'other-window) ;; TODO Adapt for terminal interface
(global-set-key (kbd "C-y") 'counsel-yank-pop)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") 'revert-buffer)

(setq ryo-excluded-modes
	  '(eshell-mode dired-mode treemacs-mode vterm-mode inferior-python-mode ediff-mode))

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

(provide 'init-modal)
