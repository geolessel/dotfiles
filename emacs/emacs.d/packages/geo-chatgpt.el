(require 'url)
(require 'json)
(require 'transient)

(defgroup elelem nil
  "Customization group for AI chat functionality."
  :group 'applications)

(defcustom elelem-provider 'anthropic
  "The AI provider to use for chat functionality."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Anthropic" anthropic))
  :group 'elelem)

(defcustom elelem-openai-model "gpt-4"
  "The OpenAI model to use for chat."
  :type 'string
  :group 'elelem)

(defcustom elelem-anthropic-model "claude-3-5-sonnet-20240620"
  "The Anthropic model to use for chat."
  :type 'string
  :group 'elelem)

(defcustom elelem-buffer-name "*LLM Chat*"
  "The buffer to build prompts and responses in."
  :type 'string
  :group 'elelem)

(defcustom elelem-system-prompts
  '(("Elixir Developer" . "You are an experienced senior developer. Your knowledge is particularly good for the Elixir language, the Phoenix framework (including LiveView) and all the available Hex packages. When you are asked for help, you provide deep idiomatic answers. Assume the user is also an experienced developer and use advanced terms and concepts in your responses. Always include a concise response along with code blocks unless asked explicitly to only include code in your response.")
    ("Emacs Developer" . "You are an experienced senior developer. Your knowledge is particularly deep for the Emacs Lisp language, the Emacs program, and various other lisps. When you are asked for help, you provide deep idiomatic responses. Assume the user is also and experienced Emacs user and use advanced terms and concepts in your responses.")
    ("Code Review" . "Please review the following code: ")
    ("Explain Concept" . "Can you explain the concept of: "))
  "List of preset prompts for AI chat."
  :type '(alist :key-type string :value-type string)
  :group 'elelem)

(defcustom elelem-current-system-prompt "Elixir Developer"
  "The system prompt that should be used for current prompts."
  :type 'string
  :group 'elelem)

(defcustom elelem-context-files nil
  "List of files to include as context for AI chat."
  :type '(repeat string)
  :group 'elelem)

(defun elelem-add-context-file ()
  "Add a file to the context files list."
  (interactive)
  (let ((file (read-file-name "Add context file: ")))
    (add-to-list 'elelem-context-files file)
    (message "Added %s to context files" file)))

(defun elelem-remove-context-file ()
  "Remove a file from the context files list."
  (interactive)
  (let ((file (completing-read "Remove context file: " elelem-context-files)))
    (setq elelem-context-files (delete file elelem-context-files))
    (message "Removed %s from context files" file)))

(defun elelem-clear-context-files ()
  "Clear all context files."
  (interactive)
  (setq elelem-context-files nil)
  (message "Cleared all context files"))

(defun elelem-chat-with-context ()
  "Prompt the user for input, then send the input and the contents of context files to the AI API."
  (interactive)
  (let* ((input (read-string "LLM prompt (with context files): "))
         (context (mapconcat (lambda (file)
                               (format "-----\nFile: %s\n\n%s\n\n"
                                       file
                                       (with-temp-buffer
                                         (insert-file-contents file)
                                         (buffer-string))))
                             elelem-context-files
                             "\n"))
         (encoded-input (encode-coding-string input 'utf-8))
         (encoded-context (encode-coding-string context 'utf-8)))
    (elelem--send-request (concat encoded-input "\n\n==============================\nCONTEXT:\n" encoded-context "END CONTEXT\n==============================\n\n"))))

(defun elelem-get-input ()
  "Prompt the user for input for the OpenAI assistant."
  (interactive)
  (let ((input (read-string "LLM prompt: ")))
    (elelem--send-request input)))

(defun elelem-get-system-prompt-choices ()
  "Get the list of system prompt choices."
  (mapcar 'car elelem-system-prompts))

(defun elelem-set-system-prompt (prompt)
  "Set the system prompt for the chat."
  (setq elelem-current-system-prompt prompt))

(defun elelem-chat-with-code ()
  "Prompt the user for input, then send the input and either the selected region or the entire buffer's contents to OpenAI's API."
  (interactive)
  (let ((input (read-string "LLM prompt (with buffer/region): "))
        (content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (elelem--send-request (concat input "\n\n==============================\nCODE:\n" content "\n==============================\n\n"))))

(defun elelem--create-input-buffer (input)
  "Create a new buffer for the user's input."
  (with-current-buffer (get-buffer-create elelem-buffer-name)
    (erase-buffer)
    (insert "# Prompt\n\n" input "\n\n")))

(defun elelem--parse-response (response)
  "Parse the AI provider's response and return the message content."
  (cond ((eq elelem-provider 'openai)
         (let* ((choices (alist-get 'choices response))
                (message-content (and choices (alist-get 'content (alist-get 'message (elt choices 0))))))
           message-content))
        ((eq elelem-provider 'anthropic)
         (let* ((content (alist-get 'content response))
                (first-message (aref content 0))
                (text (alist-get 'text first-message))
           text))
        (t (error "Unknown provider when processing response: %s" elelem-provider))))

(defun elelem--append-to-buffer (text)
  "Append TEXT to the existing buffer and save it."
  (with-current-buffer (get-buffer-create elelem-buffer-name)
    (goto-char (point-max))
    (insert "\n# Response\n\n" (or text "No valid response received.") "\n")
    (elelem--save-chat-to-file)))

(defun elelem--append-response-to-buffer (response)
  "Append the OpenAI response to the existing buffer and save it."
  (elelem--append-to-buffer response))

(defun elelem--prepare-headers ()
  (let ((api-key (elelem--get-api-key)))
    (cond ((eq elelem-provider 'openai)
           `(("Content-Type" . "application/json")
             ("Authorization" . ,(concat "Bearer " api-key))))
          ((eq elelem-provider 'anthropic)
           `(("Content-Type" . "application/json")
             ("anthropic-version" . "2023-06-01")
             ("x-api-key" . ,api-key)))
          (t (error "Unknown provider while preparing headers")))))

(defun elelem--send-request (input)
  "Send the user input to the configured AI provider's API and handle the response."
  (elelem--create-input-buffer input)
  (let* ((url-request-method "POST")
         (headers (elelem--prepare-headers))
         (url-request-extra-headers (elelem--prepare-headers))
         (url-request-data (elelem--prepare-request-data input))
         (url (elelem--get-api-url)))
    (url-retrieve url 'elelem--callback)))

(defun elelem--get-api-key ()
  "Get the API key for the configured AI provider."
  (let ((provider elelem-provider))
    (cond ((eq provider 'openai)
           (string-trim (shell-command-to-string "echo $EMACS_OPENAI_API_KEY")))
          ((eq provider 'anthropic)
           (string-trim (shell-command-to-string "echo $EMACS_ANTHROPIC_API_KEY")))
          (t (error "Unknown provider while getting api key")))))

(defun elelem--get-api-url ()
  "Get the API URL for the configured AI provider."
  (if (eq elelem-provider 'openai)
      "https://api.openai.com/v1/chat/completions"
    "https://api.anthropic.com/v1/messages"))

(defun elelem--prepare-request-data (prompt)
  "Prepare the request data for the configured AI provider."
  (let ((provider elelem-provider)
        (system-prompt (cdr (assoc elelem-current-system-prompt elelem-system-prompts))))
    (json-encode
     (cond
      ((eq provider 'openai)
       `((model . ,elelem-openai-model)
         (temperature . 1)
         (max_tokens . 3000)
         (top_p . 1)
         (frequency_penalty . 0)
         (presence_penalty . 0)
         (messages . [((role . user)
                       (content . ,prompt))])))
      ((eq provider 'anthropic)
       `((model . ,elelem-anthropic-model)
         (system . ,system-prompt)
         (messages . [((role . user)
                       (content . ,prompt))])
         (max_tokens . 3000)))
      (t (error "Unsupported provider preparing request data: %s" provider))))))

(defun elelem--callback (status &optional debug-p)
  "Handle the OpenAI API response, including debugging steps.

STATUS is the status of the network process.
If DEBUG-P is non-nil, debugging information will be printed."
  (goto-char (point-min))       ;; Move to the start of the buffer
  (search-forward "\n\n")       ;; Move past the HTTP headers
  (let ((response (buffer-substring-no-properties (point) (point-max))))
    (when debug-p
      (with-output-to-temp-buffer "*Raw LLM Response*"
        (princ response)))    ;; Print raw response for debugging
    (condition-case err
        (let* ((json-response (json-read-from-string response))
               (parsed-response (elelem--parse-response json-response)))
          (elelem--append-response-to-buffer parsed-response))
      (json-readtable-error
       (message "Failed to parse the response as JSON: %s" (error-message-string err))
       (with-output-to-temp-buffer "*LLM Response*"
         (princ "Failed to parse the response as JSON.\n")
         (princ response)))
      (error
       (message "An unexpected error occurred: %s" (error-message-string err))
       (with-output-to-temp-buffer "*LLM Response*"
         (princ "An unexpected error occurred.\n")
         (princ response))))))

(defun elelem--save-chat-to-file ()
  "Save the chat buffer to a file in the chatgpt-chats subdirectory."
  (let* ((directory (expand-file-name "chatgpt" user-emacs-directory))
         (timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (filename (expand-file-name (concat timestamp ".md") directory)))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (write-region (point-min) (point-max) filename)
    (find-file-other-window filename)
    ;; Encode the buffer to utf-8
    (set-buffer-file-coding-system 'utf-8)
    ;; Move to the response heading
    (goto-line 1)
    (re-search-forward "^# Response" nil t)
    (message "Chat saved to %s" filename)))

(defun elelem-manage-context-files ()
  "Add, remove, or clear context files"
  (interactive)
  (let ((action nil))
    (while (not (eq action ?q))
      (setq action (read-char-choice "a)dd, r)emove, c)lear, or q)uit: "
                                  '(?a ?r ?c ?q)))
    (pcase action
      (?a (call-interactively #'elelem-add-context-file))
      (?r (call-interactively #'elelem-remove-context-file))
      (?c (call-interactively #'elelem-clear-context-files))))))

;;; TRANSIENT

(transient-define-prefix elelem-menu ()
  "Transient menu for LLM interactions."
  [["Actions"
    ("c" "Chat with current (region or buffer)" elelem-chat-with-code-transient)
    ("i" "Chat with input" elelem-get-input)
    ("x" "Chat with context files" elelem-chat-with-context)]
   ["Setup"
    (elelem--infix-role)
    (elelem--infix-context-files)]])

(transient-define-infix elelem--infix-role ()
  "System role message"
  :class 'transient-lisp-variable
  :description "System role"
  :variable 'elelem-current-system-prompt
  :key "r"
  :reader (lambda (prompt &rest _)
            (let ((choices elelem-system-prompts)
                  (completion-extra-properties
                   `(:annotation-function
                     ,(lambda (choice)
                        (concat " - " (cdr (assoc choice choices)))))))
              (completing-read "System role: " (mapcar 'car elelem-system-prompts)))))

(transient-define-suffix elelem--infix-context-files ()
  :class 'transient-lisp-variable
  :variable 'elelem-context-files
  :key "f"
  :description (lambda () (format "Context files (%s selected)" (length elelem-context-files)))
  :transient t
  (interactive)
  (call-interactively #'elelem-manage-context-files)
  (transient-setup))

