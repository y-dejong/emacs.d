;; GPTel configuration and model backends

(ysd-require 'gptel)

(defvar ysd-gptel-session-buffer nil)

(setq gptel--known-backends nil)

(gptel-make-openai "Groq"
  :host "api.groq.com"
  :endpoint "/openai/v1/chat/completions"
  :stream t
  :key groq-api-key ;; Assumes variable has been set by env.el
  :models '(llama-3.3-70b-versatile
			llama-3.3-8b-instant
			(qwen-2.5-coder-32b :capabilities (tool))
			deepseek-r1-distill-llama-70b
			qwen-qwq-32b))

(gptel-make-openai "OpenRouter"
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key openrouter-api-key ;; Assumes variable has been set by env.el
  :models '(google/gemini-2.0-flash-001
			deepseek/deepseek-r1:free
			qwen/qwen-2.5-coder-32b-instruct
			anthropic/claude-3.7-sonnet:beta
			qwen/qwq-32b:free))

(push (cons 'markdown-mode "## Chatty:\n") gptel-response-prefix-alist)
(push (cons 'markdown-mode "## User:\n") gptel-prompt-prefix-alist)

(setq gptel-backend (gptel-get-backend "OpenRouter")
	  gptel-model 'qwen/qwen-2.5-coder-32b-instruct
	  gptel-use-header-line nil
	  gptel-expert-commands t
	  gptel-use-context 'user)

(add-hook 'gptel-post-stream-hook (lambda ()
									(when gptel-mode
									  (goto-char (point-max)))))

(setq-default markdown-hide-markup t ;; Hides text decoration in gptel buffer
			  markdown-fontify-code-blocks-natively t)

(keymap-set gptel-mode-map "C-<return>" 'gptel-send)
(defun setup-gptel-mode ()
  (visual-line-mode 1)
  (display-line-numbers-mode 0))
(add-hook 'gptel-mode-hook 'setup-gptel-mode)

;; Automatic file editing
(defun ysd-gptel-edit-files-from-chat ()
  "Iterates through EDIT blocks in current buffer and
  edits those buffers"
  (interactive)
  (save-excursion
	(goto-char (point-max))
	(text-property-search-backward 'gptel 'response #'eq)
	(while (re-search-forward "EDIT \\(.*?\\)\n```\\(.*?\\)\n<<<<<\\([be]?\\)\n\\(\\(?:.*\n\\)*?.*\\)\n=====\n\\(\\(?:.*\n\\)*?.*\\)\n>>>>>\n```" nil t)
	  (let* ((filename (match-string 1))
			 (code-lang (match-string 2))
			 (begin-or-end (match-string 3))
			 (old-code (match-string 4))
			 (new-code (match-string 5))
			 (code-buffer (get-file-buffer filename))
			 (edit-block (cons (match-beginning 0)(match-end 0)))
			 (highlight
			  (make-overlay (match-beginning 0) (match-end 1)))
			 edited)
		(when code-buffer
		  (overlay-put highlight 'face 'region)
		  (with-current-buffer code-buffer
			(goto-char (point-min))
			(if (search-forward old-code nil t)
				(let ((start (match-beginning 0))
					  (end (match-end 0)))
				  (when (y-or-n-p "Edit?")
					(delete-region start end)
					(goto-char start)
					(insert new-code)
					(setq edited t)))
			  (message "Failed an edit: Couldn't find match in file")))
		  (delete-overlay highlight))
		(goto-char (car edit-block))
		(delete-region (car edit-block) (cdr edit-block))
		(if edited
			(insert (format "Edited %s\n" filename))
		  (insert (format "Canceled edit of %s\n" filename)))
		(insert (format "```%s\n%s\n```" code-lang new-code))))))

(keymap-set gptel-mode-map "C-e" 'ysd-gptel-edit-files-from-chat)


;; Invocations

(setq ysd-gptel-edit-system-prompt
	  "You have the ability to make edits to files that the user provides as context.
  To edit the file, write \"EDIT filename\" where filename is the file, followed by
  a diff code block that looks like this:
  ```
  <<<<<
  <old code>
  =====
  <new code>
  >>>>>
  ```
  To make multiple edits, make separate code blocks with
  their own \"EDIT filename\" header.")

(defun ysd-gptel-ask-file (prompt &optional region-text)
  (interactive (list (read-string "Ask: ")
					 (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))))

  (unless (buffer-live-p ysd-gptel-session-buffer) (gptel ysd-gptel-session-buffer))
  (gptel-context--add-region (current-buffer) (point-min) (point-max))


  (with-current-buffer ysd-gptel-session-buffer
	(goto-char (point-max))
	(insert prompt)

	(let ((gptel--system-message
		   (concat gptel--system-message ysd-gptel-edit-system-prompt))
		  (gptel-context-wrap-function
		   (if region-text
			   (lambda (message contexts)
				 (concat
				  (gptel-context--wrap-default message contexts)
				  "The following text is highlighted:\n"
				  region-text))
			 #'gptel-context--wrap-default)))
	  (gptel-send)))
  (gptel-context-remove)
  (unless (get-buffer-window ysd-gptel-session-buffer)
	(display-buffer ysd-gptel-session-buffer gptel-display-buffer-action)))

(defun ysd-gptel-code-at-point (prompt)
  (interactive (list (read-string "Ask: ")))
  (insert "{{USER CURSOR HERE}}")
  (gptel-context--add-region (current-buffer) (point-min) (point-max))
  (gptel-request prompt
	:stream t
	:system "You are a coding assistant inside Emacs. Respond only with code that will be inserted inside the provided text file at the location `{{USER CURSOR HERE}}`. Indent and format the code properly to be inserted at that point.")
  (delete-backward-char (length "{{USER CURSOR HERE}}"))
  (gptel-context-remove))

;; Transient menu for activation
;; Stolen and stripped from gptel--infix-provider in gptel-transient.el
(defun ysd-gptel-choose-model ()
  (interactive)
  (cl-loop
   for (name . backend) in gptel--known-backends
   nconc (cl-loop for model in (gptel-backend-models backend)
				  collect (list (concat name ":" (gptel--model-name model))
								backend model))
   into models-alist
   with completion-extra-properties =
   `(:annotation-function
	 ,(lambda (comp)
		(let* ((model (nth 2 (assoc comp models-alist)))
			   (desc (get model :description))
			   (caps (get model :capabilities))
			   (context (get model :context-window))
			   (input-cost (get model :input-cost))
			   (output-cost (get model :output-cost))
			   (cutoff (get model :cutoff-date)))
		  (when (or context input-cost output-cost)
			(concat
			 " " (propertize " " 'display `(space :align-to 34))
			 (when context (format "%5dk" context))
			 " " (propertize " " 'display `(space :align-to 42))
			 (when input-cost (format "$%5.2f in" input-cost))
			 (if (and input-cost output-cost) "," " ")
			 " " (propertize " " 'display `(space :align-to 53))
			 (when output-cost (format "$%6.2f out" output-cost)))))))
   finally return
   (cdr (assoc (completing-read "Model:" models-alist nil t nil nil
								(concat (gptel-backend-name gptel-backend) ":"
										(gptel--model-name gptel-model)))
			   models-alist))))

;; Stolen from gptel-menu in gptel-transient.el
(defun ysd-gptel-choose-session ()
  (interactive)
  (read-buffer
   "GPTel Session:" (generate-new-buffer-name
					 (concat "*" (gptel-backend-name gptel-backend) "*"))
   nil (lambda (buf-name)
		 (if (consp buf-name) (setq buf-name (car buf-name)))
		 (let ((buf (get-buffer buf-name)))
		   (and (buffer-local-value 'gptel-mode buf)
				(not (eq (current-buffer) buf)))))))

(transient-define-prefix ysd-gptel-menu ()
  [["Setup"
	("m"
	 "Model"
	 (lambda ()
	   (interactive)
	   (let ((model (ysd-gptel-choose-model)))
		 (setq gptel-model (cadr model)
			   gptel-backend (car model)))))
	("s" "GPTel Session"
	 (lambda ()
	   (interactive)
	   (setq ysd-gptel-session-buffer (ysd-gptel-choose-session))))]
   ["Context"
	("f" "Add File" gptel-add-file)
	("b" "Add Buffer" (lambda () (interactive) (gptel-add '(4))))
	("a" "Add context at point" gptel-add)
	("C" "Clear context" gptel-context-remove-all)]
   ["Invoke"
	("p" "Insert code at point" ysd-gptel-code-at-point)
	("c" "Open Chat" (lambda () (interactive) (gptel ysd-gptel-session-buffer nil nil t)))
	("q" "Ask about file" ysd-gptel-ask-file)]])

(ryo-modal-key "!" 'ysd-gptel-menu)

(provide 'init-ai)
