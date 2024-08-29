(require 'url)
(require 'json)
(require 'transient)

(defgroup elelem nil
  "Customization group for AI chat functionality."
  :group 'applications)

(defcustom elelem-buffer-name "*elelem chat*"
  "The buffer to build prompts and responses in."
  :type 'string
  :group 'elelem)

(defcustom elelem-system-prompts
  '(("Elixir Developer" . "You are an experienced senior developer. Your knowledge is particularly good for the Elixir language, the Phoenix framework (including LiveView) and all the available Hex packages. When you are asked for help, you provide deep idiomatic answers. Assume the user is also an experienced developer and use advanced terms and concepts in your responses. Always include a concise response along with code blocks unless asked explicitly to only include code in your response.")
    ("Emacs Developer" . "You are an experienced senior software developer. Your knowledge is particularly deep for the Emacs Lisp language, the Emacs program, and various other lisps. When you are asked for help, you provide deep idiomatic responses directly related to Emacs, Emacs Lisp, and other lisps. If required, use your knowledge of other programming languages as well. Assume the user is also and experienced Emacs user and use advanced terms and concepts in your responses.")
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

(defcustom elelem-autosave-chats-dir "~/emacs.d/elelem-chats"
  "Which directory to autosave a response from a provider.

Set to `nil' to disable autosaving."
  :type '(choice (const :tag "Don't autosave" nil)
                 (string :tag "Autosave directory"))
  :group 'elelem)

(defcustom elelem-available-models
  '((openai . gpt-4o)
    (openai . gpt-4o-mini)
    (anthropic . claude-3-5-sonnet-20240620)
    (anthropic . claude-3-haiku-20240307))
  "The available providers and models for interactions."
  :type '(alist :key-type symbol :key-type symbol)
  :group 'elelem)

(defcustom elelem-current-provider-and-model '(anthropic . claude-3-5-sonnet-20240620)
  "The current provider and model for interactions."
  :type '(alist :key-type symbol :key-type symbol)
  :group 'elelem)

;;; FUNCTIONS

(defun elelem-add-context-file ()
  "Add a file to the context files list."
  (interactive)
  (let ((file (read-file-name "Add context file: ")))
    (add-to-list 'elelem-context-files file)
    (message "Added %s to elelem context files" file)))

(defun elelem-remove-context-file ()
  "Remove a file from the context files list."
  (interactive)
  (let ((file (completing-read "Remove context file: " elelem-context-files)))
    (setq elelem-context-files (delete file elelem-context-files))
    (message "Removed %s from elelem context files" file)))

(defun elelem-clear-context-files ()
  "Clear all context files."
  (interactive)
  (setq elelem-context-files nil)
  (message "Cleared all elelem context files"))

(defun elelem-chat-with-context ()
  "Prompt the user for input, then send the input and the contents of context files to the AI API."
  (interactive)
  (let* ((model (cdr elelem-current-provider-and-model))
         (input (read-string (format "%s prompt (with context files): " model)))
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
  (let* ((model (cdr elelem-current-provider-and-model))
         (input (read-string (format "%s prompt: " model))))
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
  (let* ((model (cdr elelem-current-provider-and-model))
         (input (read-string (format "%s prompt (with buffer/region): " model)))
         (content (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (elelem--send-request (concat input "\n\n==============================\nCODE:\n" content "\n==============================\n\n"))))

(defun elelem--create-input-buffer (input)
  "Create a new buffer for the user's input."
  (let ((buffer (get-buffer-create elelem-buffer-name)))
    (with-current-buffer buffer
      (set-buffer-file-coding-system 'utf-8)
      (markdown-mode)
      (visual-line-mode 1)
      (erase-buffer)
      (insert "# Prompt\n\n" input "\n\n# Response" (format " (%s %s)" (car elelem-current-provider-and-model) (cdr elelem-current-provider-and-model)) "\n\n")
      (display-buffer buffer))
    (let ((window (get-buffer-window buffer)))
      (with-selected-window window
        (set-buffer-file-coding-system 'utf-8)
        (re-search-forward "^# Response" nil t)
        ;; make the response display at the top of the window
        (recenter 1)))))

(defun elelem--parse-response (response)
  "Parse the AI provider's response and return the message content."
  (let ((provider (car elelem-current-provider-and-model)))
    (pcase provider
      ('openai
       (let* ((choices (alist-get 'choices response))
              (first-choice (aref choices 0))
              (delta (alist-get 'delta first-choice))
              (content (alist-get 'content delta)))
         content))
      ('anthropic
       (let* ((delta (alist-get 'delta response))
              (text (alist-get 'text delta)))
         text))
      (_ (error "Unknown provider when processing response from provider: %s" provider)))))

(defun elelem--append-to-buffer (text)
  "Append TEXT to the existing buffer."
  (with-current-buffer (get-buffer elelem-buffer-name)
    (goto-char (point-max))
    (insert (or text "No valid response received.\n"))
    (redisplay)))

(defun elelem--append-response-to-buffer (response)
  "Append the OpenAI response to the existing buffer and save it."
  (elelem--append-to-buffer response))

(defun elelem--prepare-headers ()
  (let ((api-key (elelem--get-api-key))
        (provider (car elelem-current-provider-and-model)))
    (pcase provider
      ('openai
       `(("Content-Type" . "application/json")
         ("Authorization" . ,(concat "Bearer " api-key))))
      ('anthropic
       `(("Content-Type" . "application/json")
         ("anthropic-version" . "2023-06-01")
         ("x-api-key" . ,api-key)))
      (_ (error "Unknown provider while preparing headers for provider: %s" provider)))))

(defun elelem--prepare-request-data (prompt)
  "Prepare the request data for the configured AI provider."
  (let ((provider (car elelem-current-provider-and-model))
        (model (cdr elelem-current-provider-and-model))
        (system-prompt (cdr (assoc elelem-current-system-prompt elelem-system-prompts))))
    (json-encode
     (pcase provider
       ('openai
        `((model . ,(symbol-name model))
          (stream . t)
          (temperature . 1)
          (max_tokens . 3000)
          (top_p . 1)
          (frequency_penalty . 0)
          (presence_penalty . 0)
          (messages . [((role . user)
                        (content . ,(encode-coding-string prompt 'utf-8)))])))
       ('anthropic
        `((model . ,(symbol-name model))
          (stream . t)
          (system . ,system-prompt)
          (messages . [((role . user)
                        (content . ,(encode-coding-string prompt 'utf-8)))])
          (max_tokens . 3000)))
       (_ (error "Unsupported provider preparing request data: %s" provider))))))

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
  (let ((provider (car elelem-current-provider-and-model)))
    (pcase provider
      ('openai (string-trim (shell-command-to-string "echo $EMACS_OPENAI_API_KEY")))
      ('anthropic (string-trim (shell-command-to-string "echo $EMACS_ANTHROPIC_API_KEY")))
      (_ (error "Unknown provider while getting api key: %s" provider)))))

(defun elelem--get-api-url ()
  "Get the API URL for the configured AI provider."
  (let ((provider (car elelem-current-provider-and-model)))
    (pcase provider
      ('openai "https://api.openai.com/v1/chat/completions")
      ('anthropic "https://api.anthropic.com/v1/messages")
      (_ (error "Unknown provider while getting API URL: %s provider")))))

(defun elelem--callback (status &optional debug-p)
  "Handle the OpenAI API response, including debugging steps.

STATUS is the status of the network process.
If DEBUG-P is non-nil, debugging information will be printed."
  (goto-char (point-min))       ; Move to the start of the buffer
  (search-forward "\n\n")       ; Move past the HTTP headers
  (let ((inhibit-read-only t)   ; Allow modifications to the buffer
        (json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (point) (line-end-position))))
        (when (string-prefix-p "data: " line)
          (let ((json-data (substring line 6)))
            (unless (string= json-data "[DONE]")
              (condition-case err
                  (let* ((json-response (json-read-from-string json-data))
                         (content (elelem--parse-response json-response)))
                    (when content (elelem--append-response-to-buffer content)))
                (json-readtable-error (message "Failed to parse the response as JSON: %s" (error-message-string err)))
                (error (message "An unexpected error occurred while processing chunk: %s" (error-message-string err)))))))
        (forward-line 1)))
    (elelem--save-chat-to-file)))

(defun elelem--save-chat-to-file ()
  "Save the chat buffer to a file in the `elelem-autosave-chats-dir' directory."
  (when elelem-autosave-chats-dir
    (let* ((directory (expand-file-name elelem-autosave-chats-dir))
           (timestamp (format-time-string "%Y%m%d_%H%M%S"))
           (filename (expand-file-name (concat timestamp ".md") directory)))
      (unless (file-exists-p directory)
        (make-directory directory t))
      (with-current-buffer (get-buffer elelem-buffer-name)
        ;; Encode the buffer to utf-8
        (set-buffer-file-coding-system 'utf-8)
        ;; write contents of buffer to filename; don't append; visit
        (write-region (point-min) (point-max) filename nil t))
      (message "Chat saved to %s" filename))))

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
    ("c" "Chat with current (region or buffer)" elelem-chat-with-code)
    ("x" "Chat with context files" elelem-chat-with-context)
    ("i" "Chat with input" elelem-get-input)]
   ["Setup"
    (elelem--infix-provider-and-model)
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

(transient-define-infix elelem--infix-provider-and-model ()
  "Select provider and model"
  :class 'transient-lisp-variable
  :variable 'elelem-current-provider-and-model
  :key "m"
  :reader (lambda (&rest _)
            (let* ((choices (mapcar (lambda (pair)
                                      (cons (format "%s %s" (car pair) (cdr pair)) pair))
                                    elelem-available-models))
                   (selection (completing-read "Provider and model: "
                                               (mapcar #'car choices) nil t)))
              (cdr (assoc selection choices)))))
