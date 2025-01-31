;;; org-ai-openai.el --- OpenAI related functions  -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; org-ai.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-ai.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-ai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains the OpenAI API related functions for org-ai.

;;; Code:

(require 'org)
(require 'org-element)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'gv)
(require 'json)

(require 'org-ai-block)

(defcustom org-ai-jump-to-end-of-block t
  "If non-nil, jump to the end of the block after inserting the completion."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-auto-fill nil
  "If non-nil, will fill paragraphs when inserting completions."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-openai-api-token ""
  "This is your OpenAI API token.
You need to specify if you do not store the token in
`auth-sources'. You can retrieve it at
https://platform.openai.com/account/api-keys."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-use-auth-source t
  "If non-nil, use auth-source to retrieve the OpenAI API token.
The secret should be stored in the format
  machine api.openai.com login org-ai password <your token>
in the `auth-sources' file."
  :type 'boolean
  :group 'org-ai)

(defcustom org-ai-default-completion-model "text-davinci-003"
  "The default model to use for completion requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-chat-model "gpt-4o-mini"
  "The default model to use for chat-gpt requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-chat-models '("gpt-4o-mini"
                                "gpt-4"
                                "gpt-4-32k"
                                "gpt-4-turbo"
                                "gpt-4o"
                                "gpt-3.5-turbo"
                                "o1"
                                "o1-preview"
                                "o1-mini"
                                "o3-mini"
                                "chatgpt-4o-latest")
  "Alist of available chat models. See https://platform.openai.com/docs/models."
  :type '(alist :key-type string :value-type string)
  :group 'org-ai)

(defun org-ai--check-model (model endpoint)
  "Check if the model name is somehow mistyped.
`MODEL' is the model name. `ENDPOINT' is the API endpoint."
  (unless model
    (error "No org-ai model specified."))

  (when (or (string-match-p "api.openai.com" endpoint)
            (string-match-p "openai.azure.com" endpoint))

    (let ((lowercased (downcase model)))
      (when (and (string-prefix-p "gpt-" model) (not (string-equal lowercased model)))
        (warn "Model name '%s' should be lowercase. Use '%s' instead." model lowercased)))

    (unless (member model org-ai-chat-models)
      (message "Model '%s' is not in the list of available models. Maybe this is because of a typo or maybe we haven't yet added it to the list. To disable this message add (add-to-list 'org-ai-chat-models \"%s\") to your init file." model model))))

(defcustom org-ai-default-max-tokens nil
  "The default maximum number of tokens to generate. This is what costs money."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-chat-system-prompt "You are a helpful assistant inside Emacs."
  "The system message helps set the behavior of the assistant:
https://platform.openai.com/docs/guides/chat/introduction. This
default prompt is send as the first message before any user (ME)
or assistant (AI) messages. Inside a +#begin_ai...#+end_ai block
you can override it with: '[SYS]: <your prompt>'."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-inject-sys-prompt-for-all-messages nil
  "Wether to add the `org-ai-default-chat-system-prompt' before all user messages.

By default the system prompt is only added before the first
message.

You can set this to true for a single block using the
:sys-everywhere option on the #+begin_ai block.

This can be useful to enforce the behavior specified by this
messages."
  :type 'string
  :group 'org-ai)

(make-obsolete-variable 'org-ai-default-inject-sys-prompt-for-all-messages
                        "With newer ChatGPT versions this is no longer necessary."
                        "2023-12-26")

(defun org-ai--read-service-name (name)
  "Map a service name such as 'openai' to a valid `org-ai-service' symbol."
  (intern-soft name))

(defcustom org-ai-service 'openai
  "Service to use. Either openai or azure-openai."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Azure-OpenAI" azure-openai)
                 (const :tag "perplexity.ai" perplexity.ai)
                 (const :tag "anthropic" anthropic))
  :group 'org-ai)

(defvar org-ai-openai-chat-endpoint "https://api.openai.com/v1/chat/completions")

(defvar org-ai-openai-completion-endpoint "https://api.openai.com/v1/completions")

;; Azure-Openai specific variables

(defcustom org-ai-azure-openai-api-base "https://your-instance.openai.azure.com"
  "Base API URL for Azure-OpenAI."
  :type 'string
  :group 'org-ai)

;; Additional Azure-Openai specific variables
(defcustom org-ai-azure-openai-deployment "azure-openai-deployment-name"
  "Deployment name for Azure-OpenAI API."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-azure-openai-api-version "2023-07-01-preview"
  "API version for Azure-OpenAI."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-anthropic-api-version "2023-06-01"
  "API version for api.anthropic.com."
  :type 'string
  :group 'org-ai)

(defun org-ai--openai-get-token (&optional service)
  "Try to get the openai token.
Either from `org-ai-openai-api-token' or from auth-source."
  (or (and
       (stringp org-ai-openai-api-token)
       (not (string-empty-p org-ai-openai-api-token))
       org-ai-openai-api-token)
      (and
       org-ai-use-auth-source
       (org-ai--openai-get-token-auth-source service))
      (error "Please set `org-ai-openai-api-token' to your OpenAI API token or setup auth-source (see org-ai readme)")))

(defun org-ai--openai-get-token-auth-source (&optional service)
  "Retrieves the authentication token for the OpenAI service using auth-source."
  (require 'auth-source)
  (let* ((service (or service org-ai-service))
         (endpoint (pcase service
                     ('openai "api.openai.com")
                     ('perplexity.ai "api.perplexity.ai")
                     ('anthropic "api.anthropic.com")
                     ('azure-openai (strip-api-url org-ai-azure-openai-api-base)))))
    (or (auth-source-pick-first-password :host endpoint :user "org-ai")
        (auth-source-pick-first-password :host endpoint :login "org-ai"))))

(defun strip-api-url (url)
  "Strip the leading https:// and trailing / from an URL"
  (let ((stripped-url (if (string-prefix-p "https://" url)
                          (substring url 8)
                        url)))
    (if (string-suffix-p "/" stripped-url)
        (substring stripped-url 0 -1)
      stripped-url)))

(defun org-ai--get-endpoint (messages &optional service)
  "Determine the correct endpoint based on the service and
whether messages are provided."
  (let ((service (or service org-ai-service)))
    (cond
     ((eq service 'azure-openai)
      (format "%s/openai/deployments/%s%s/completions?api-version=%s"
	      org-ai-azure-openai-api-base org-ai-azure-openai-deployment
	      (if messages "/chat" "") org-ai-azure-openai-api-version))
     ((eq service 'perplexity.ai)
      "https://api.perplexity.ai/chat/completions")
     ((eq service 'anthropic)
      "https://api.anthropic.com/v1/messages")
     (t
      (if messages org-ai-openai-chat-endpoint org-ai-openai-completion-endpoint)))))

(defun org-ai--get-headers (&optional service)
  "Determine the correct headers based on the service."
  (let ((service (or service org-ai-service)))
    `(("Content-Type" . "application/json")
      ,@(cond
        ((eq service 'azure-openai)
         `(("api-key" . ,(org-ai--openai-get-token service))))
        ((eq service 'anthropic)
         `(("x-api-key" . ,(org-ai--openai-get-token service))
           ("anthropic-version" . ,org-ai-anthropic-api-version)))
        (t
         `(("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,(org-ai--openai-get-token service)) " ") 'utf-8))))))))

(defvar org-ai--current-request-buffer-for-stream nil
  "Internal var that stores the current request buffer.
For stream responses.")

(defvar org-ai--current-request-buffer nil
  "Internal var that stores the current request buffer.
For chat completion responses.")

(defvar org-ai--current-request-callback nil
  "Internal var that stores the current request callback.")

(defvar org-ai--current-request-is-streamed nil
  "Whether we expect a streamed response or a single completion payload.")

(defvar org-ai--current-progress-reporter nil
  "progress-reporter for non-streamed responses to make them less boring.")

(defvar org-ai--current-progress-timer nil
  "Timer for updating the progress reporter for non-streamed responses to make them less boring.")

(defvar org-ai-after-chat-insertion-hook nil
  "Hook that is called when a chat response is inserted.
Note this is called for every stream response so it will typically
only contain fragments.")

(defvar org-ai--current-insert-position-marker nil
  "Where to insert the result.")
(make-variable-buffer-local 'org-ai--current-insert-position-marker)

(defvar org-ai--current-chat-role nil
  "During chat response streaming, this holds the role of the \"current speaker\".")

(defvar org-ai--chat-got-first-response nil)
(make-variable-buffer-local 'org-ai--chat-got-first-response)

(defvar org-ai--currently-inside-code-markers nil)
(make-variable-buffer-local 'org-ai--currently-inside-code-markers)

(defvar org-ai--url-buffer-last-position-marker nil
  "Local buffer var to store last read position.")
;; (make-variable-buffer-local 'org-ai--url-buffer-last-position-marker)
;; (makunbound 'org-ai--url-buffer-last-position-marker)

(defvar org-ai--debug-data nil)
(defvar org-ai--debug-data-raw nil)

;; (with-current-buffer "*scratch*"
;;   (erase-buffer)
;;   (pop-to-buffer "*scratch*" t)
;;   (let ((n 16))
;;    (insert (car (nth n org-ai--debug-data-raw)))
;;    (goto-char (cadr (nth n org-ai--debug-data-raw)))
;;    (beginning-of-line)))

(cl-defun org-ai-stream-completion (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty service context)
  "Start a server-sent event stream.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `MODEL' is the model to use. `MAX-TOKENS' is the maximum
number of tokens to generate. `TEMPERATURE' is the temperature of
the distribution. `TOP-P' is the top-p value. `FREQUENCY-PENALTY'
is the frequency penalty. `PRESENCE-PENALTY' is the presence
penalty. `CONTEXT' is the context of the special block. Service
is the ai cloud service such as 'openai or 'azure-openai."
  (let* ((context (or context (org-ai-special-block)))
         (buffer (current-buffer))
         (info (org-ai-get-block-info context)))
    (cl-macrolet ((let-with-captured-arg-or-header-or-inherited-property
                    (definitions &rest body)
                    `(let ,(cl-loop for (sym . default-form) in definitions
                                    collect `(,sym (or ,sym
                                                       (alist-get ,(intern (format ":%s" (symbol-name sym))) info)
                                                       (when-let ((prop (org-entry-get-with-inheritance ,(symbol-name sym))))
                                                         (if (eq (quote ,sym) 'model)
                                                             prop
                                                           (if (stringp prop) (string-to-number prop) prop)))
                                                       ,@default-form)))
                       ,@body)))
      (let-with-captured-arg-or-header-or-inherited-property
       ((model (if messages org-ai-default-chat-model org-ai-default-completion-model))
        (max-tokens org-ai-default-max-tokens)
        (top-p)
        (temperature)
        (frequency-penalty)
        (presence-penalty)
        (service))
       (let ((callback (cond
                        (messages (lambda (result) (org-ai--insert-stream-response context buffer result t)))
                        (t (lambda (result) (org-ai--insert-single-response context buffer result))))))
         (org-ai-stream-request :prompt prompt
                                :messages messages
                                :model model
                                :max-tokens max-tokens
                                :temperature temperature
                                :top-p top-p
                                :frequency-penalty frequency-penalty
                                :presence-penalty presence-penalty
                                :service service
                                :callback callback))))))

(defun org-ai--insert-single-response (context buffer &optional response)
  "Insert the response from the OpenAI API into the buffer.
`CONTEXT' is the context of the special block. `BUFFER' is the
buffer to insert the response into. `RESPONSE' is the response
from the OpenAI API."
  (when response
    (if-let ((error (plist-get response 'error)))
        (if-let ((message (plist-get error 'message))) (error message) (error error))
      (if-let* ((choice (aref (plist-get response 'choices) 0))
                (text (plist-get choice 'text)))
          (with-current-buffer buffer
            ;; set mark so we can easily select the generated text (e.g. to delet it to try again)
            (unless org-ai--current-insert-position-marker
              (push-mark (org-element-property :contents-end context)))
            (let ((pos (or (and org-ai--current-insert-position-marker
                                (marker-position org-ai--current-insert-position-marker))
                           (org-element-property :contents-end context))))
              (save-excursion
                (goto-char pos)

                (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                  (insert "\n")
                  (backward-char))
                (insert text)
                (setq org-ai--current-insert-position-marker (point-marker)))))))))


(cl-deftype org-ai--response-type ()
  '(member role text stop error))

(cl-defstruct org-ai--response
  (type (:type org-ai--response-type))
  payload)

;; Here is an example for how a full sequence of OpenAI responses looks like:
;; '((id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (role "assistant" content "") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content "Hello") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content ",") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content " Robert") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta nil logprobs nil finish_reason "stop")])
;;   nil)
;;
;; and Anthropic:
;; '((type "message_start" message (id "msg_01HoMq4LgkUpHpkXqXoZ7R1W" type "message" role "assistant" model "claude-3-5-sonnet-20240620" content [] stop_reason nil stop_sequence nil usage (input_tokens 278 output_tokens 2)))
;;   (type "content_block_start" index 0 content_block (type "text" text ""))
;;   (type "ping")
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "Hello Robert"))
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "."))
;;   (type "content_block_stop" index 0)
;;   (type "message_delta" delta (stop_reason "end_turn" stop_sequence nil) usage (output_tokens 22))
;;   (type "message_stop"))
(defun org-ai--normalize-response (response)
  "This function normalizes JSON data received from OpenAI-style, Anthropic, and Perplexity endpoints.
`RESPONSE' is one JSON message of the stream response."

  (if-let ((error-message (plist-get response 'error)))
      (list (make-org-ai--response :type 'error :payload (or (plist-get error 'message) error-message)))

    (let ((response-type (plist-get response 'type)))

      ;; first try anthropic
      (cond
       ((string= response-type "ping") nil)
       ((string= response-type "message_start")
        (when-let ((role (plist-get (plist-get response 'message) 'role)))
          (list (make-org-ai--response :type 'role :payload role))))
       ((string= response-type "content_block_start")
        (when-let ((text (plist-get (plist-get response 'content_block) 'text)))
          (list (make-org-ai--response :type 'text :payload text))))
       ((string= response-type "content_block_delta")
        (when-let ((text (plist-get (plist-get response 'delta) 'text)))
          (list (make-org-ai--response :type 'text :payload text))))
       ((string= response-type "content_block_stop") nil)
       ((string= response-type "message_delta")
        (when-let ((stop-reason (plist-get (plist-get response 'delta) 'stop_reason)))
          (list (make-org-ai--response :type 'stop :payload stop-reason))))
       ((string= response-type "message_stop") nil)


       ;; try perplexity.ai
       ((and (plist-get response 'model) (string-prefix-p "llama-" (plist-get response 'model)))
        (let ((choices (plist-get response 'choices)))
          (when (and choices (> (length choices) 0))
            (let* ((choice (aref choices 0))
                   (message (plist-get choice 'message))
                   (delta (plist-get choice 'delta))
                   (role (or (plist-get delta 'role) (plist-get message 'role)))
                   (content (or (plist-get delta 'content) (plist-get message 'content)))
                   (finish-reason (plist-get choice 'finish_reason)))
              (append
               (when role
                 (list (make-org-ai--response :type 'role :payload role)))
               (when content
                 (list (make-org-ai--response :type 'text :payload content)))
               (when finish-reason
                 (list (make-org-ai--response :type 'stop :payload finish-reason))))))))

       ;; single message e.g. from non-streamed completion
       ((let ((choices (plist-get response 'choices)))
          (and (= 1 (length choices))
               (plist-get (aref choices 0) 'message)))
        (let* ((choices (plist-get response 'choices))
               (choice (aref choices 0))
               (text (plist-get (plist-get choice 'message) 'content))
               (role (plist-get (plist-get choice 'message) 'role))
               (finish-reason (or (plist-get choice 'finish_reason) 'stop)))
          (list (make-org-ai--response :type 'role :payload role)
                (make-org-ai--response :type 'text :payload text)
                (make-org-ai--response :type 'stop :payload finish-reason))))

       ;; try openai streamed
       (t (let ((choices (plist-get response 'choices)))
            (cl-loop for choice across choices
                     append (or (when-let ((role (plist-get (plist-get choice 'delta) 'role)))
                                  (list (make-org-ai--response :type 'role :payload role)))
				(when-let ((role (plist-get (plist-get choice 'delta) 'content)))
                                  (list (make-org-ai--response :type 'text :payload role)))
                                (when-let ((content (plist-get (plist-get choice 'delta) 'content)))
                                  (list (make-org-ai--response :type 'text :payload content)))
                                (when-let ((finish-reason (plist-get choice 'finish_reason)))
                                  (list (make-org-ai--response :type 'stop :payload finish-reason)))))))))))

(defun org-ai--insert-stream-response (context buffer &optional response insert-role)
  "`RESPONSE' is one JSON message of the stream response.
When `RESPONSE' is nil, it means we are done. `CONTEXT' is the
context of the special block. `BUFFER' is the buffer to insert
the response into."
  (let ((normalized (org-ai--normalize-response response)))
   (cl-loop for response in normalized
            do (let ((type (org-ai--response-type response)))
                 (when (eq type 'error)
                   (error (org-ai--response-payload response)))

                 (with-current-buffer buffer
                   (let ((pos (or (and org-ai--current-insert-position-marker
                                       (marker-position org-ai--current-insert-position-marker))
                                  (and context (org-element-property :contents-end context))
                                  (point))))
                     (save-excursion
                       (goto-char pos)

                       ;; make sure we have enough space at end of block, don't write on same line
                       (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                         (insert "\n")
                         (backward-char)))

                     (cl-case type

                       (role (let ((role (org-ai--response-payload response)))
                               (when (not (string= role org-ai--current-chat-role))
                                 (save-excursion
                                   (goto-char pos)

                                   (setq org-ai--current-chat-role role)
                                   (let ((role (and insert-role (org-ai--response-payload response))))
                                     (cond
                                      ((string= role "assistant")
                                       (insert "\n[AI]: "))
                                      ((string= role "user")
                                       (insert "\n[ME]: "))
                                      ((string= role "system")
                                       (insert "\n[SYS]: ")))
                                     (run-hook-with-args 'org-ai-after-chat-insertion-hook 'role role)
                                     (setq org-ai--current-insert-position-marker (point-marker)))))))

                       (text (let ((text (org-ai--response-payload response)))
                               (save-excursion
                                 (goto-char pos)
                                 (when (or org-ai--chat-got-first-response (not (string= (string-trim text) "")))
                                   (when (and (not org-ai--chat-got-first-response) (string-prefix-p "```" text))
                                     ;; start markdown codeblock responses on their own line
                                     (insert "\n"))
                                   ;; track if we are inside code markers
                                   (setq org-ai--currently-inside-code-markers (and (not org-ai--currently-inside-code-markers)
                                                                                    (string-match-p "```" text)))
                                   (insert (decode-coding-string text 'utf-8))
                                   ;; "auto-fill"
                                   (when (and org-ai-auto-fill (not org-ai--currently-inside-code-markers))
                                     (fill-paragraph))
                                   ;; hook
                                   (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text))
                                 (setq org-ai--chat-got-first-response t)
                                 (setq org-ai--current-insert-position-marker (point-marker)))))

                       (stop (progn
                               (save-excursion
                                 (when org-ai--current-insert-position-marker
                                   (goto-char org-ai--current-insert-position-marker))

                                 ;; (message "inserting user prompt: %" (string= org-ai--current-chat-role "user"))
                                 (let ((text (if insert-role
                                                 (let ((text "\n\n[ME]: "))
                                                   (insert text)
                                                   text)
                                               "")))
                                   (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end text)
                                   (setq org-ai--current-insert-position-marker (point-marker))))

                               (org-element-cache-reset)
                               (when org-ai-jump-to-end-of-block (goto-char org-ai--current-insert-position-marker)))))))))
   normalized))

(cl-defun org-ai-stream-request (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty service callback)
  "Send a request to the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `CALLBACK' is the callback function. `MODEL' is the
model to use. `MAX-TOKENS' is the maximum number of tokens to
generate. `TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value. `FREQUENCY-PENALTY' is the frequency
penalty. `PRESENCE-PENALTY' is the presence penalty."
  (setq org-ai--current-insert-position-marker nil)
  (setq org-ai--chat-got-first-response nil)
  (setq org-ai--debug-data nil)
  (setq org-ai--debug-data-raw nil)
  (setq org-ai--currently-inside-code-markers nil)
  (setq service (or (if (stringp service) (org-ai--read-service-name service) service)
                    org-ai-service))
  (setq stream (org-ai--stream-supported service model))

  (let* ((url-request-extra-headers (org-ai--get-headers service))
         (url-request-method "POST")
         (endpoint (org-ai--get-endpoint messages service))
         (url-request-data (org-ai--payload :prompt prompt
					    :messages messages
					    :model model
					    :max-tokens max-tokens
					    :temperature temperature
					    :top-p top-p
					    :frequency-penalty frequency-penalty
					    :presence-penalty presence-penalty
                                            :service service
                                            :stream stream)))
    (org-ai--check-model model endpoint)

    ;; (message "REQUEST %s %s" endpoint url-request-data)

    (setq org-ai--current-request-is-streamed stream)
    (setq org-ai--current-request-callback callback)
    (when (not stream) (org-ai--progress-reporter-until-request-done))

    (setq org-ai--current-request-buffer-for-stream
          (url-retrieve
           endpoint
           (lambda (_events)
             (with-current-buffer org-ai--current-request-buffer-for-stream
               (org-ai--url-request-on-change-function nil nil nil))
             (org-ai--maybe-show-openai-request-error org-ai--current-request-buffer-for-stream)
             (org-ai-reset-stream-state))))

    ;; (display-buffer-use-some-window org-ai--current-request-buffer-for-stream nil)

    (if stream
        (unless (member 'org-ai--url-request-on-change-function after-change-functions)
          (with-current-buffer org-ai--current-request-buffer-for-stream
            (add-hook 'after-change-functions #'org-ai--url-request-on-change-function nil t)))
      (with-current-buffer org-ai--current-request-buffer-for-stream
        (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)))

    org-ai--current-request-buffer-for-stream))

(defun org-ai--maybe-show-openai-request-error (request-buffer)
  "If the API request returned an error, show it.
`REQUEST-BUFFER' is the buffer containing the request."
  (with-current-buffer request-buffer
    (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
      (goto-char url-http-end-of-headers))
    (condition-case nil
        (when-let* ((body (json-read))
                    (err (or (alist-get 'error body)
                             (plist-get body 'error)))
                    (message (or (alist-get 'message err)
                                 (plist-get err 'message)))
                    (message (if (and message (not (string-blank-p message)))
                                 message
                               (json-encode err))))
          (org-ai--show-error message))
      (error nil))))

(defun org-ai--show-error (error-message)
  "Show an error message in a buffer.
`ERROR-MESSAGE' is the error message to show."
  (condition-case nil
      (let ((buf (get-buffer-create "*org-ai error*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "Error from the service API:\n\n")
          (insert error-message)
          (display-buffer buf)
          (goto-char (point-min))
          (toggle-truncate-lines -1)
          (read-only-mode 1)
          ;; close buffer when q is pressed
          (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer)))
          t))
    (error nil)))

(cl-defun org-ai--payload (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty service stream)
  "Create the payload for the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `MODEL' is the model to use. `MAX-TOKENS' is the
maximum number of tokens to generate. `TEMPERATURE' is the
temperature of the distribution. `TOP-P' is the top-p value.
`FREQUENCY-PENALTY' is the frequency penalty. `PRESENCE-PENALTY'
is the presence penalty.
`STREAM' is a boolean indicating whether to stream the response."
  (let ((extra-system-prompt)
        (max-completion-tokens))

    (when (eq service 'anthropic)
      (when (string-equal (plist-get (aref messages 0) :role) "system")
        (setq extra-system-prompt (plist-get (aref messages 0) :content))
        (cl-shiftf messages (cl-subseq messages 1)))
      (setq max-tokens (or max-tokens 4096)))

    ;; o1 models currently does not support system prompt
    (when (and (or (eq service 'openai) (eq service 'azure-openai))
               (or (string-prefix-p "o1" model) (string-prefix-p "o3" model)))
      (setq messages (cl-remove-if (lambda (msg) (string-equal (plist-get msg :role) "system")) messages))
      ;; o1 does not support max-tokens
      (when max-tokens
        (setq max-tokens nil)
        (setq max-completion-tokens (or max-tokens 128000))))

   (let* ((input (if messages `(messages . ,messages) `(prompt . ,prompt)))
          ;; TODO yet unsupported properties: n, stop, logit_bias, user
          (data (map-filter (lambda (x _) x)
                            `(,input
                              (model . ,model)
                              ,@(when stream                `((stream . ,stream)))
                              ,@(when max-tokens            `((max_tokens . ,max-tokens)))
                              ,@(when max-completion-tokens `((max-completion-tokens . ,max-completion-tokens)))
                              ,@(when temperature           `((temperature . ,temperature)))
                              ,@(when top-p                 `((top_p . ,top-p)))
                              ,@(when frequency-penalty     `((frequency_penalty . ,frequency-penalty)))
                              ,@(when presence-penalty      `((presence_penalty . ,presence-penalty)))))))

     (when extra-system-prompt
       (setq data (append data `((system . ,extra-system-prompt)))))

     (encode-coding-string (json-encode data) 'utf-8))))

(defun org-ai--url-request-on-change-function (_beg _end _len)
  "Look into the url-request buffer and manually extracts JSON stream responses.
Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range."
  (with-current-buffer org-ai--current-request-buffer-for-stream
    (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
      (save-match-data
        (save-excursion
          (if org-ai--url-buffer-last-position-marker
              (goto-char org-ai--url-buffer-last-position-marker)
            (goto-char url-http-end-of-headers)
            (setq org-ai--url-buffer-last-position-marker (point-marker)))

          ;; Avoid a bug where we skip responses because url has modified the http
          ;; buffer and we are not where we think we are.
          ;; TODO this might break
          (unless (eolp)
            (beginning-of-line))

          (let ((errored nil))
            ;; (setq org-ai--debug-data-raw
            ;;       (append org-ai--debug-data-raw
            ;;               (list
            ;;                (list (buffer-substring-no-properties (point-min) (point-max))
            ;;                      (point)))))

            ;; handle completion (non-streamed) response of a single json object
            (while (and (not org-ai--current-request-is-streamed)
                        (not errored))
              (let ((json-object-type 'plist)
                    (json-key-type 'symbol)
                    (json-array-type 'vector))
                (condition-case _err
                    (let ((data (json-read)))
                      (when org-ai--current-request-callback
                        (funcall org-ai--current-request-callback data)))
                  (error
                   (setq errored t))))
              (progn
                (when org-ai--current-request-callback
                  (funcall org-ai--current-request-callback nil))
                (org-ai-reset-stream-state)
                (message "org-ai request done")))

            ;; handle stream completion response, multiple json objects prefixed with "data: "
            (while (and org-ai--current-request-is-streamed
                        (not errored)
                        (search-forward "data: " nil t))
              (let* ((line (buffer-substring-no-properties (point) (line-end-position))))
                ;; (message "...found data: %s" line)
                (if (string= line "[DONE]")
                    (progn
                      (when org-ai--current-request-callback
                        (funcall org-ai--current-request-callback nil))
                      (set-marker org-ai--url-buffer-last-position-marker (point))
                      (org-ai-reset-stream-state)
                      (message "org-ai request done"))
                  (let ((json-object-type 'plist)
                        (json-key-type 'symbol)
                        (json-array-type 'vector))
                    (condition-case _err
                        (let ((data (json-read-from-string line)))
			  (end-of-line)
                          ;; (setq org-ai--debug-data (append org-ai--debug-data (list data)))
                          (when org-ai--current-request-callback
                            (funcall org-ai--current-request-callback data))
                          (set-marker org-ai--url-buffer-last-position-marker (point)))
                      (error
                       (setq errored t)
                       (when org-ai--url-buffer-last-position-marker
                         (goto-char org-ai--url-buffer-last-position-marker))))))))))))))

(defun org-ai--stream-supported (service model)
  "Check if the stream is supported by the service and model.
`SERVICE' is the service to use. `MODEL' is the model to use."
  ;; stream not supported by openai o1 models
  (not (and (or (eq service 'openai) (eq service 'azure-openai))
            (or (string-prefix-p "o1" model) (string-prefix-p "o3" model)))))

(defun org-ai-interrupt-current-request ()
  "Interrupt the current request."
  (interactive)
  (when (and org-ai--current-request-buffer-for-stream (buffer-live-p org-ai--current-request-buffer-for-stream))
    (let (kill-buffer-query-functions)
      (kill-buffer org-ai--current-request-buffer-for-stream))
    (setq org-ai--current-request-buffer-for-stream nil)
    (org-ai-reset-stream-state)))

(defun org-ai-reset-stream-state ()
  "Reset the stream state."
  (interactive)
  (when (and org-ai--current-request-buffer-for-stream (buffer-live-p org-ai--current-request-buffer-for-stream))
    (with-current-buffer org-ai--current-request-buffer-for-stream
      (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)
      (setq org-ai--url-buffer-last-position-marker nil)))
  (setq org-ai--current-request-callback nil)
  (setq org-ai--url-buffer-last-position-marker nil)
  (setq org-ai--current-chat-role nil)
  (setq org-ai--current-request-is-streamed nil)
  (org-ai--progress-reporter-cancel))

(defcustom org-ai--witty-messages
  '("Pondering imponderables... Almost there!"
    "`grep`ing the neural net for answers..."
    "Fetching witty AI response... In the meantime, have you tried Vim? Just kidding!"
    "Teaching AI the ways of the Lisp."
    "Consulting the sacred parentheses."
    "Hold tight! The AI is garbage collecting its thoughts."
    "Fetching clever reply from `/dev/ai`."
    "The AI is busy counting parentheses. Almost there!"
    "Running in an infinite loop... Just kidding! Processing your request."
    "The AI is stuck in a `(cl-labels ((loop () (loop))) (loop))`... Wait, no it's not.")
  "Messages to entertain while waiting")

(defun org-ai--progress-reporter-until-request-done ()
  (org-ai--progress-reporter-cancel)
  (setq org-ai--current-progress-reporter
        (let ((msg (or
                    (nth (random (length org-ai--witty-messages)) org-ai--witty-messages)
                    "Waiting for a response")))
          (make-progress-reporter msg)))

  (setq org-ai--current-progress-timer
        (let ((counter 0)
              (reporter org-ai--current-progress-reporter))
          (run-with-timer
           1.0 0.2
           (lambda ()
             (setq counter (1+ counter))
             (progress-reporter-update reporter))))))

(defun org-ai--progress-reporter-cancel ()
  (when org-ai--current-progress-reporter
    (progress-reporter-done org-ai--current-progress-reporter)
    (setq org-ai--current-progress-reporter nil))
  (when org-ai--current-progress-timer
    (cancel-timer org-ai--current-progress-timer)))

(defun org-ai-open-request-buffer ()
  "A debug helper that opens the url request buffer."
  (interactive)
  (when (buffer-live-p org-ai--current-request-buffer-for-stream)
    (pop-to-buffer org-ai--current-request-buffer-for-stream)))

(defun org-ai-switch-chat-model ()
  "Change `org-ai-default-chat-model'."
  (interactive)
  (let ((model (completing-read "Model: " org-ai-chat-models nil t)))
    (setq org-ai-default-chat-model model)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-openai)

;;; org-ai-openai.el ends here
