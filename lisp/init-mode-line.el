;; Use telephone-line to show status info,
;; and use header line instead of mode line

(ysd-require 'telephone-line)

(defun ysd-make-header-line-mouse-map (mouse function)
  (let ((map (make-sparse-keymap)))
	(define-key map (vector 'header-line mouse) function)
	map))

;;;;; SET COLORS ;;;;;

;;(set-face-attribute 'mode-line nil :background (catppuccin-get-color 'overlay1))
(set-face-attribute 'telephone-line-evil-normal nil :foreground (catppuccin-get-color 'red) :background (catppuccin-get-color 'base))
(set-face-attribute 'telephone-line-evil-insert nil :foreground (catppuccin-get-color 'green) :background (catppuccin-get-color 'base))

(defface ysd-tele-line-modified
  `((t (:foreground ,(catppuccin-get-color 'red) :background ,(catppuccin-get-color 'surface2))))
  "Telephone line modified face"
  :group 'telephone-line)

(defface ysd-surface2-bg
  `((t (:background ,(catppuccin-get-color 'surface2))))
  "Surface2 background face"
  :group 'telephone-line)

(defface ysd-surface1-bg
  `((t :background ,(catppuccin-get-color 'surface1)))
  "Surface1 background face"
  :group 'telephone-line)

(defface ysd-invisible
  `((t (:foreground ,(catppuccin-get-color 'base) :background ,(catppuccin-get-color 'base))))
  "Surface1 background face"
  :group 'telephone-line)

(defun ysd-tele-line-surface1-face (active)
  (cond ((not active) 'mode-line-inactive)
		(t 'ysd-surface1-bg)))

(defun ysd-tele-line-surface2-face (active)
  (cond ((not active) 'mode-line-inactive)
		(t 'ysd-surface2-bg)))


(defun ysd-tele-line-buffer-face (active)
  'ysd-invisible)

(telephone-line-defsegment* ysd-buffer-segment ()
  " ")

;;;;; CREATE SEGMENTS ;;;;;

(push '(surface2 . ysd-tele-line-surface2-face) telephone-line-faces)
(push '(surface1 . ysd-tele-line-surface1-face) telephone-line-faces)
(push '(buffer . ysd-tele-line-buffer-face) telephone-line-faces)

(defun ysd-tele-line-modified-face (active)
  (cond ((not active) 'mode-line-inactive)
		((buffer-modified-p) 'ysd-tele-line-modified)
		(t 'ysd-surface2-bg)))
(push '(modif . ysd-tele-line-modified-face) telephone-line-faces)

(defun ysd-modal-face (active)
  "Return an appropriate face depending whether ryo-modal is activated, given whether frame is ACTIVE."
  (cond ((not active) 'ysd-invisible)
		((not (boundp 'ryo-modal-mode)) 'mode-line)
		((not ryo-modal-mode) 'telephone-line-evil-insert)
		(t 'telephone-line-evil-normal)))
(push '(ysd-modal . ysd-modal-face) telephone-line-faces)

(telephone-line-defsegment* ysd-ryo-modal-segment ()
  "◉")

(defun ysd-set-coding-system (e)
  (interactive "e")
  (with-selected-window (posn-window (event-start e))
	(call-interactively 'set-buffer-file-coding-system)))

(telephone-line-defsegment* ysd-telephone-line-encoding-segment ()
  (propertize
   (upcase (symbol-name
			(plist-get (coding-system-plist buffer-file-coding-system) :name)))
   'help-echo "Buffer coding system:\nmouse-1: Change"
   'local-map (ysd-make-header-line-mouse-map
			   'mouse-1 (lambda (e)
						  (interactive "e")
						  (with-selected-window (posn-window (event-start e))
							(call-interactively 'set-buffer-file-coding-system))))
   'mouse-face 'mode-line-highlight))


(telephone-line-defsegment* ysd-telephone-line-eol-segment ()
  (propertize
   (pcase (coding-system-eol-type buffer-file-coding-system)
	 (0 "LF")
	 (1 "CRLF")
	 (2 "CR"))
   'help-echo "End-of-line style:\nmouse-1: Cycle"
   'local-map (ysd-make-header-line-mouse-map
			   'mouse-1 'mode-line-change-eol)
   'mouse-face 'mode-line-highlight))

;;;;; CREATE SEPARATORS ;;;;;

(defvar telephone-line-halfcircle-right
  (make-instance 'telephone-line-separator
				 :axis-func (lambda (x) (let ((result (sqrt (- 9.869 (expt x 2)))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-right))

(defvar telephone-line-halfcircle-left
  (make-instance 'telephone-line-separator
				 :axis-func (lambda (x) (let ((result (- (sqrt (- 9.869 (expt x 2))))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-left))

(defvar telephone-line-halfcircle-hollow-right
  (make-instance 'telephone-line-subseparator
				 :axis-func (lambda (x) (let ((result (sqrt (- 9.869 (expt x 2)))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-hollow-right))

(defvar telephone-line-halfcircle-hollow-left
  (make-instance 'telephone-line-subseparator
				 :axis-func (lambda (x) (let ((result (- (sqrt (- 9.869 (expt x 2))))))
										  (if (isnan result) 0 result)))
				 :alt-separator telephone-line-utf-abs-hollow-left))

;;;;; ASSEMBLE SEGMENTS ;;;;;

(setq
 telephone-line-lhs
 '((ysd-modal . (ysd-ryo-modal-segment))
   (modif . (telephone-line-buffer-name-segment))
   (surface1 . (telephone-line-major-mode-segment
				telephone-line-minor-mode-segment)))
 telephone-line-rhs
 '((surface1 . (ysd-telephone-line-encoding-segment))
   (surface2 . (ysd-telephone-line-eol-segment))
   (buffer . (ysd-buffer-segment)))

 telephone-line-target 'header-line ;; TODO disable header-line option in Emacs <28
 telephone-line-primary-left-separator telephone-line-halfcircle-left
 telephone-line-primary-right-separator telephone-line-halfcircle-right)

(setq-default mode-line-format nil)
(telephone-line-mode 1)

(defun swap-header-and-mode-line (symbol newval operation where)
  (with-current-buffer where
	(when (and (eq operation 'set) (not (eq newval (default-value 'header-line-format))))
	  (setq mode-line-format newval)
	  (run-with-timer 0 nil (lambda () (setq header-line-format (default-value 'header-line-format)))))))
;; run-with-timer 0 waits until after function ends to change header-line-format back to the original value

(add-variable-watcher 'header-line-format 'swap-header-and-mode-line)

;; Hide modes with diminish
(ysd-require 'diminish)
(let ((diminished-modes
	   '(ivy-mode ryo-modal-mode which-key-mode)))
  (dolist (mode diminished-modes)
	(diminish mode)))

(provide 'init-mode-line)
