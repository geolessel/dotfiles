(require 'url)
(require 'json)

(defun geo/chatgpt-get-input ()
  "Prompt the user for input for the OpenAI assistant."
  (interactive)
  (let ((input (read-string "Ask OpenAI: ")))
    (geo/chatgpt--send-request input)))

(defun geo/chatgpt-chat-with-code ()
  "Prompt the user for input, then send the input and either the selected region or the entire buffer's contents to OpenAI's API."
  (interactive)
  (let ((input (read-string "Ask OpenAI (with buffer/region): "))
        (content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (geo/chatgpt--send-request (concat input "\n\n==============================\nCODE:\n" content "==============================\n\n"))))

(defun geo/chatgpt--create-input-buffer (input)
  "Create a new buffer for the user's input."
  (with-current-buffer (get-buffer-create "*OpenAI Chat*")
    (erase-buffer)
    (insert "# Prompt\n\n" input "\n\n")))

(defun geo/chatgpt--parse-response (response)
  "Parse the OpenAI response and return the message content."
  (let* ((choices (alist-get 'choices response))
         (message-content (and choices (alist-get 'content (alist-get 'message (elt choices 0))))))
    message-content))

(defun geo/chatgpt--append-to-buffer (text)
  "Append TEXT to the existing buffer and save it."
  (with-current-buffer (get-buffer-create "*OpenAI Chat*")
    (goto-char (point-max))
    (insert "\n# Response\n\n" (or text "No valid response received.") "\n")
    (geo/chatgpt--save-chat-to-file)))

(defun geo/chatgpt--append-response-to-buffer (response)
  "Append the OpenAI response to the existing buffer and save it."
  (let* ((message-content (geo/chatgpt--parse-response response)))
    (geo/chatgpt--append-to-buffer message-content)))

(defun geo/chatgpt--send-request (input)
  "Send the user input to OpenAI's API and handle the response."
  (geo/chatgpt--create-input-buffer input)
  (let* ((api-key (string-trim (shell-command-to-string "echo $EMACS_OPENAI_API_KEY")))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " api-key))))
         (url-request-data
          (json-encode `(("model" . "gpt-4o")
                         ("temperature" . 1)
                         ("max_tokens" . 3000)
                         ("top_p" . 1)
                         ("frequency_penalty" . 0)
                         ("presence_penalty" . 0)
                         ("messages" . [((role . "system")
                                         (content .
                                                  [((type . "text")
                                                    (text . "You are a helpful senior software developer. You have many, many
years of experience writing software for many different kinds of
environments. Despite your experience, you are still up to date
on the latest language idioms, security considerations,
architecture, and language features.\n\nWhen you are presented
with a question about specific code, give a helpful response in
easy to understand language. When asked for more details, go deep
into the topic.\n\nWhen presented with software architecture
questions, respond without code examples unless code is
specifically asked for. Limit your responses to the programming
language the user is using."))]))
                                        ((role . "user")
                                         (content . [((type . "text")
                                                      (text . ,input))]))]))))
         (url "https://api.openai.com/v1/chat/completions"))
    (url-retrieve url 'geo/chatgpt--callback)))

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
        (let ((json-response (json-read-from-string response)))
          (geo/chatgpt--append-response-to-buffer json-response))
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
