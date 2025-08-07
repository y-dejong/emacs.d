(defgroup eyesaver nil
  "Eyesaver timer to remind the user to take eye breaks."
  :prefix "eyesaver-"
  :group 'convenience)

(defface eyesaver-active
  '((t :foreground "chartreuse" :weight normal))
  "Face for active timer in mode line."
  :group 'eyesaver)

(defface eyesaver-break
  '((t :foreground "red" :weight bold))
  "Face for break time in mode line."
  :group 'eyesaver)

(defcustom eyesaver-duration 1200
  "Default timer duration in seconds."
  :type 'integer
  :group 'eyesaver)

(defvar eyesaver-timer nil)
(defvar eyesaver-remaining 0)
(defvar eyesaver-mode-line-string ""
  "Variable to display the countdown and break time in the mode line.")

;; Format seconds to MM:SS
(defun eyesaver-format-time (seconds)
  (let ((minutes (floor seconds 60)))
    (format "%d:%02d" minutes (% seconds 60))))

;; Update mode line and check for break time
(defun eyesaver-tick ()
  (setq eyesaver-remaining (1- eyesaver-remaining))
  (cond
   ((<= eyesaver-remaining 0)
    (when (timerp eyesaver-timer)
      (cancel-timer eyesaver-timer))
    (setq eyesaver-mode-line-string
          (propertize "BREAK TIME" 'face 'eyesaver-break)))
   (t
    (setq eyesaver-mode-line-string
          (propertize (eyesaver-format-time eyesaver-remaining)
                      'face 'eyesaver-active))))
  (force-mode-line-update))

;; Command to set the timer
(defun eyesaver-set (seconds)
  "Set eyesaver timer for SECONDS (default: `eyesaver-duration')."
  (interactive
   (list (read-number "Set eyesaver (seconds): " eyesaver-duration)))
  (setq eyesaver-duration seconds)
  (when (timerp eyesaver-timer)
    (cancel-timer eyesaver-timer))
  (setq eyesaver-remaining seconds)
  (setq eyesaver-timer (run-at-time t 1 #'eyesaver-tick)))

;; Add to mode line
(setq-default mode-line-format
              (cons '(:eval (when (and eyesaver-remaining (> eyesaver-remaining 0))
                             (propertize " | [Eyesaver: " 'face 'mode-line-bold)))
                    mode-line-format))
(setq-default mode-line-format
              (cons '(" " eyesaver-mode-line-string " ") mode-line-format))
(setq-default mode-line-format
              (cons '(:eval (when (equal eyesaver-mode-line-string
                                 (propertize "BREAK TIME" 'face 'eyesaver-break))
                             (propertize " ] " 'face 'mode-line-bold)))
                    mode-line-format))
