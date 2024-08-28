(require 'url)
(require 'json)

(defgroup geo-ai-chat nil
  "Customization group for AI chat functionality."
  :group 'applications)

(defcustom geo-ai-chat-provider 'anthropic
  "The AI provider to use for chat functionality."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Anthropic" anthropic))
  :group 'geo-ai-chat)

(defcustom geo-ai-chat-openai-model "gpt-4"
  "The OpenAI model to use for chat."
  :type 'string
  :group 'geo-ai-chat)

(defcustom geo-ai-chat-anthropic-model "claude-3-5-sonnet-20240620"
  "The Anthropic model to use for chat."
  :type 'string
  :group 'geo-ai-chat)

(defun geo/chatgpt-get-input ()
  "Prompt the user for input for the OpenAI assistant."
  (interactive)
  (let ((input (read-string "LLM prompt: ")))
    (geo/chatgpt--send-request input)))

(defun geo/chatgpt-chat-with-code ()
  "Prompt the user for input, then send the input and either the selected region or the entire buffer's contents to OpenAI's API."
  (interactive)
  (let ((input (read-string "Ask OpenAI (with buffer/region): "))
        (content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (geo/chatgpt--send-request (concat input "\n\n==============================\nCODE:\n" content "\n==============================\n\n"))))

(defun geo/chatgpt--create-input-buffer (input)
  "Create a new buffer for the user's input."
  (with-current-buffer (get-buffer-create "*OpenAI Chat*")
    (erase-buffer)
    (insert "# Prompt\n\n" input "\n\n")))

(defun geo/chatgpt--parse-response (response)
  "Parse the AI provider's response and return the message content."
  (message "Response: %s" response)
  (message "geo-ai-chat-provider: %s" geo-ai-chat-provider)
  (cond ((eq geo-ai-chat-provider 'openai)
         (message "provider: openai")
         (let* ((choices (alist-get 'choices response))
                (message-content (and choices (alist-get 'content (alist-get 'message (elt choices 0))))))
           message-content))
        ((eq geo-ai-chat-provider 'anthropic)
         (message "provider: anthropic")
         (let* ((content (alist-get 'content response))
                (_ (message "content: %s" content))
                (first-message (aref content 0))
                (_ (message "first-message: %s" first-message))
                (text (alist-get 'text first-message))
                (_ (message "text: %s" text)))
           ;; (message "Content: %s || first-message: %s || text: %s" content first-message text)
           text))
        (t (error "Unknown provider when processing response: %s" geo-ai-chat-provider))))

(defun geo/chatgpt--append-to-buffer (text)
  "Append TEXT to the existing buffer and save it."
  (with-current-buffer (get-buffer-create "*OpenAI Chat*")
    (goto-char (point-max))
    (insert "\n# Response\n\n" (or text "No valid response received.") "\n")
    (geo/chatgpt--save-chat-to-file)))

(defun geo/chatgpt--append-response-to-buffer (response)
  "Append the OpenAI response to the existing buffer and save it."
  (geo/chatgpt--append-to-buffer response))

(defun geo/chatgpt--prepare-headers ()
  (let ((api-key (geo/chatgpt--get-api-key)))
    (cond ((eq geo-ai-chat-provider 'openai)
           `(("Content-Type" . "application/json")
             ("Authorization" . ,(concat "Bearer " api-key))))
          ((eq geo-ai-chat-provider 'anthropic)
           `(("Content-Type" . "application/json")
             ("anthropic-version" . "2023-06-01")
             ("x-api-key" . ,api-key)))
          (t (error "Unknown provider while preparing headers")))))

(defun geo/chatgpt--send-request (input)
  "Send the user input to the configured AI provider's API and handle the response."
  (geo/chatgpt--create-input-buffer input)
  (let* ((url-request-method "POST")
         (headers (geo/chatgpt--prepare-headers))
         (url-request-extra-headers (geo/chatgpt--prepare-headers))
         (url-request-data (geo/chatgpt--prepare-request-data input))
         (url (geo/chatgpt--get-api-url)))
    (url-retrieve url 'geo/chatgpt--callback)))

(defun geo/chatgpt--get-api-key ()
  "Get the API key for the configured AI provider."
  (let ((provider geo-ai-chat-provider))
    (cond ((eq provider 'openai)
           (string-trim (shell-command-to-string "echo $EMACS_OPENAI_API_KEY")))
          ((eq provider 'anthropic)
           (string-trim (shell-command-to-string "echo $EMACS_ANTHROPIC_API_KEY")))
          (t (error "Unknown provider while getting api key")))))

(defun geo/chatgpt--get-api-url ()
  "Get the API URL for the configured AI provider."
  (if (eq geo-ai-chat-provider 'openai)
      "https://api.openai.com/v1/chat/completions"
    "https://api.anthropic.com/v1/messages"))

(defun geo/chatgpt--prepare-request-data (prompt)
  "Prepare the request data for the configured AI provider."
  (let ((provider geo-ai-chat-provider))
    (json-encode
     (cond
      ((eq provider 'openai)
       `((model . ,geo-ai-chat-openai-model)
         (temperature . 1)
         (max_tokens . 3000)
         (top_p . 1)
         (frequency_penalty . 0)
         (presence_penalty . 0)
         (messages . [((role . user)
                       (content . ,prompt))])))
      ((eq provider 'anthropic)
       `((model . ,geo-ai-chat-anthropic-model)
         (messages . [((role . user)
                       (content . ,prompt))])
         (max_tokens . 3000)))
      (t (error "Unsupported provider: %s" provider))))))

(defun geo/chatgpt--callback (status &optional debug-p)
  "Handle the OpenAI API response, including debugging steps.

STATUS is the status of the network process.
If DEBUG-P is non-nil, debugging information will be printed."
  (goto-char (point-min))       ;; Move to the start of the buffer
  (search-forward "\n\n")       ;; Move past the HTTP headers
  (let ((response (buffer-substring-no-properties (point) (point-max))))
    (when debug-p
      (with-output-to-temp-buffer "*Raw OpenAI Response*"
        (princ response)))    ;; Print raw response for debugging
    (condition-case err
        (let* ((json-response (json-read-from-string response))
               (parsed-response (geo/chatgpt--parse-response json-response)))
          (message "Parsed: %s" parsed-response)
          (geo/chatgpt--append-response-to-buffer parsed-response))
      (json-readtable-error
       (message "Failed to parse the response as JSON: %s" (error-message-string err))
       (with-output-to-temp-buffer "*OpenAI Response*"
         (princ "Failed to parse the response as JSON.\n")
         (princ response)))
      (error
       (message "An unexpected error occurred: %s" (error-message-string err))
       (with-output-to-temp-buffer "*OpenAI Response*"
         (princ "An unexpected error occurred.\n")
         (princ response))))))

(defun geo/chatgpt--save-chat-to-file ()
  "Save the chat buffer to a file in the chatgpt-chats subdirectory."
  (let* ((directory (expand-file-name "chatgpt" user-emacs-directory))
         (timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (filename (expand-file-name (concat timestamp ".md") directory)))
    (unless (file-exists-p directory)
      (make-directory directory t))
    (write-region (point-min) (point-max) filename)
    (find-file-other-window filename)
    (message "Chat saved to %s" filename)))
