;; Completion setup using ivy + counsel + swiper

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

(provide 'init-completion)
