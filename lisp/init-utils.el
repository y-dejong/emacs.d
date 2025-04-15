;;;;; KILL RING FUNCTIONS ;;;;;

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

;; TODO make ysd-bookmark-push and ysd-bookmark-pop
;; To make point-to-register and jump-to-register function like the kill ring

;;;;; EDITING AND NAVIGATION ;;;;;

(defun insert-line-below ()
  (interactive)
  (end-of-line)
  (newline))


(defun beginning-or-indentation ()
  "Jump to beginning of line after whitespace. Press multiple times
  to move before/after whitespace."
  (interactive)
  (cond
   ((bolp) (forward-to-indentation 0))
   ((save-excursion (skip-chars-backward " \t") (bolp)) (beginning-of-line))
   (t (back-to-indentation))))

(defun backward-same-syntax ()
  (interactive "^") ;; Just added this because it's used in forward-same-syntax and I want to match its behavior
  (forward-same-syntax -1))

(defun insert-one-char (char)
  (interactive "c")
  (insert-char char))

(defun replace-one-char (char)
  (interactive "c")
  (delete-char 1)
  (insert-char char))

;; Stolen from https://www.emacswiki.org/emacs/NavigatingParentheses
(defun jump-to-paren-match (&optional arg)
  "Jump to matching parenthesis according to `show-paren-mode'.

	When called with a prefix or EDIT is t, jump to matching
	parenthesis such that insertion will happen inside the list.

	\(fn &optional EDIT)"
  (interactive "p")
  ;; data is either nil or a list of form:
  ;;     (HERE-BEG HERE-END THERE-BEG THERE-END MISMATCH)
  (let ((data (show-paren--default))
		(edit (not (eq arg 1))))
	(when data
	  ;; Found a parenthesis
	  (let* ((here-beg (nth 0 data))
			 (here-end (nth 1 data))
			 (there-beg (nth 2 data))
			 (there-end (nth 3 data))
			 (mismatch (nth 4 data)))
		(if (not mismatch)
			;; At parenthesis with a match
			(cond ((<= (point) here-beg)  ; at opening
				   (goto-char there-end)
				   (if edit (backward-char 1)))
				  ((goto-char there-beg)  ; at closing
				   (if edit (forward-char 1)))))))))

;;;;; KEYMAPS ;;;;;

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

(provide 'init-utils)
