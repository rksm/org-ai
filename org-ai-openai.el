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

(defcustom org-ai-openai-api-token nil
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

(defun org-ai--openai-get-token ()
  "Try to get the openai token.
Either from `org-ai-openai-api-token' or from auth-source."
  (or org-ai-openai-api-token
      (when org-ai-use-auth-source
        (require 'auth-source)
        (auth-source-pick-first-password :host "api.openai.com" :user "org-ai"))
      (error "Please set `org-ai-openai-api-token' to your OpenAI API token or setup auth-source (see org-ai readme)")))

(defcustom org-ai-default-completion-model "text-davinci-003"
  "The default model to use for completion requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-chat-model "gpt-3.5-turbo"
  "The default model to use for chat-gpt requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-chat-models '("gpt-3.5-turbo"
                                "gpt-3.5-turbo-16k"
                                "gpt-4"
                                "gpt-4-32k"
                                "gpt-4-vision-preview"
                                "gpt-4-1106-preview")
  "Alist of available chat models. See https://platform.openai.com/docs/models."
  :type '(alist :key-type string :value-type string)
  :group 'org-ai)

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

(defcustom org-ai-service 'openai
  "Service to use. Either openai or azure-openai."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Azure-OpenAI" azure-openai))
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

(defun org-ai--get-endpoint (messages)
  "Determine the correct endpoint based on the service and
whether messages are provided."
  (cond
   ((eq org-ai-service 'azure-openai)
    (format "%s/openai/deployments/%s%s/completions?api-version=%s"
	    org-ai-azure-openai-api-base org-ai-azure-openai-deployment
	    (if messages "/chat" "") org-ai-azure-openai-api-version))
   (t
    (if messages org-ai-openai-chat-endpoint org-ai-openai-completion-endpoint))))

(defun org-ai--get-headers ()
  "Determine the correct headers based on the service."
  `(("Content-Type" . "application/json")
    ,(cond
      ((eq org-ai-service 'azure-openai)
       `("api-key" . ,(org-ai--openai-get-token)))
      (t
       `("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,(org-ai--openai-get-token)) " ") 'utf-8))))))

(defvar org-ai--current-request-buffer-for-stream nil
  "Internal var that stores the current request buffer.
For stream responses.")

(defvar org-ai--current-request-buffer nil
  "Internal var that stores the current request buffer.
For chat completion responses.")

(defvar org-ai--current-request-callback nil
  "Internal var that stores the current request callback.")

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

(cl-defun org-ai-stream-completion (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty context)
  "Start a server-sent event stream.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `MODEL' is the model to use. `MAX-TOKENS' is the maximum
number of tokens to generate. `TEMPERATURE' is the temperature of
the distribution. `TOP-P' is the top-p value. `FREQUENCY-PENALTY'
is the frequency penalty. `PRESENCE-PENALTY' is the presence
penalty. `CONTEXT' is the context of the special block."
  (let ((context (or context (org-ai-special-block)))
        (buffer (current-buffer)))
    (let* ((info (org-ai-get-block-info context))
           (model (or model (alist-get :model info) (if messages org-ai-default-chat-model org-ai-default-completion-model)))
           (max-tokens (or max-tokens (alist-get :max-tokens info) org-ai-default-max-tokens))
           (top-p (or top-p (alist-get :top-p info)))
           (temperature (or temperature (alist-get :temperature info)))
           (frequency-penalty (or frequency-penalty (alist-get :frequency-penalty info)))
           (presence-penalty (or presence-penalty (alist-get :presence-penalty info)))
           (callback (if messages
                         (lambda (result) (org-ai--insert-chat-completion-response context buffer result))
                       (lambda (result) (org-ai--insert-stream-completion-response context buffer result)))))
      (setq org-ai--current-insert-position-marker nil)
      (setq org-ai--chat-got-first-response nil)
      (setq org-ai--debug-data nil)
      (setq org-ai--debug-data-raw nil)
      (org-ai-stream-request :prompt prompt
                             :messages messages
                             :model model
                             :max-tokens max-tokens
                             :temperature temperature
                             :top-p top-p
                             :frequency-penalty frequency-penalty
                             :presence-penalty presence-penalty
                             :callback callback))))

(defun org-ai--insert-stream-completion-response (context buffer &optional response)
  "Insert the response from the OpenAI API into the buffer.
`CONTEXT' is the context of the special block. `BUFFER' is the
buffer to insert the response into. `RESPONSE' is the response
from the OpenAI API."
  (if response
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

(defun org-ai--insert-chat-completion-response (context buffer &optional response)
  "`RESPONSE' is one JSON message of the stream response.
When `RESPONSE' is nil, it means we are done. `CONTEXT' is the
context of the special block. `BUFFER' is the buffer to insert
the response into."
  (if response

      ;; process response
      (if-let ((error (plist-get response 'error)))
          (if-let ((message (plist-get error 'message))) (error message) (error error))
        (with-current-buffer buffer
          (let ((pos (or (and org-ai--current-insert-position-marker
                              (marker-position org-ai--current-insert-position-marker))
                         (org-element-property :contents-end context))))
            (save-excursion
              (goto-char pos)

              ;; make sure we have enough space at end of block, don't write on same line
              (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                (insert "\n")
                (backward-char))

              ;; insert text
              (if-let* ((choices (or (alist-get 'choices response)
                                     (plist-get response 'choices)))
                        (choice (and (arrayp choices) (> (length choices) 0) (aref choices 0)))
                        (delta (plist-get choice 'delta)))
                  (cond
                   ((plist-get delta 'role)
                    (let ((role (plist-get delta 'role)))
                      (progn
                        (setq org-ai--current-chat-role role)
                        (cond
                         ((string= role "assistant")
                          (insert "\n[AI]: "))
                         ((string= role "user")
                          (insert "\n[ME]: "))
                         ((string= role "system")
                          (insert "\n[SYS]: ")))
                        (run-hook-with-args 'org-ai-after-chat-insertion-hook 'role role))))
                   ((plist-get delta 'content)
                    (let ((text (plist-get delta 'content)))
                      (when (or org-ai--chat-got-first-response (not (string= (string-trim text) "")))
                        (when (and (not org-ai--chat-got-first-response) (string-prefix-p "```" text))
                          ;; start markdown codeblock responses on their own line
                          (insert "\n"))
                        (insert (decode-coding-string text 'utf-8))
                        (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text))
                      (setq org-ai--chat-got-first-response t)))))

              (setq org-ai--current-insert-position-marker (point-marker))))))

    ;; insert new prompt and change position
    (with-current-buffer buffer
      (when org-ai--current-insert-position-marker
        (goto-char org-ai--current-insert-position-marker))
      (let ((text "\n\n[ME]: "))
        (insert text)
        (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end text))
      (org-element-cache-reset))))

(cl-defun org-ai-stream-request (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty callback)
  "Send a request to the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `CALLBACK' is the callback function. `MODEL' is the
model to use. `MAX-TOKENS' is the maximum number of tokens to
generate. `TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value. `FREQUENCY-PENALTY' is the frequency
penalty. `PRESENCE-PENALTY' is the presence penalty."
  (let* ((url-request-extra-headers (org-ai--get-headers))
         (url-request-method "POST")
         (endpoint (org-ai--get-endpoint messages))
         (url-request-data (org-ai--payload :prompt prompt
					    :messages messages
					    :model model
					    :max-tokens max-tokens
					    :temperature temperature
					    :top-p top-p
					    :frequency-penalty frequency-penalty
					    :presence-penalty presence-penalty
                                            :stream t)))

    ;; (message "REQUEST %s" url-request-data)

    (setq org-ai--current-request-callback callback)

    (setq org-ai--current-request-buffer-for-stream
          (url-retrieve
           endpoint
           (lambda (_events)
             (org-ai--maybe-show-openai-request-error org-ai--current-request-buffer-for-stream)
             (org-ai-reset-stream-state))))

    ;; (display-buffer-use-some-window org-ai--current-request-buffer-for-stream nil)

    (unless (member 'org-ai--url-request-on-change-function after-change-functions)
      (with-current-buffer org-ai--current-request-buffer-for-stream
        (add-hook 'after-change-functions #'org-ai--url-request-on-change-function nil t)))

    org-ai--current-request-buffer-for-stream))

(cl-defun org-ai-chat-request (&optional &key messages model max-tokens temperature top-p frequency-penalty presence-penalty callback)
  "Send a request to the OpenAI API. Do not stream.
`MESSAGES' is the query for chatgpt.
`CALLBACK' is the callback function.
`MODEL' is the model to use.
`MAX-TOKENS' is the maximum number of tokens to generate.
`TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value.
`FREQUENCY-PENALTY' is the frequency penalty.
`PRESENCE-PENALTY' is the presence penalty."
  (let* ((url-request-extra-headers (org-ai--get-headers))
         (url-request-method "POST")
         (endpoint (org-ai--get-endpoint messages))
         (url-request-data (org-ai--payload :messages messages
					    :model model
					    :max-tokens max-tokens
					    :temperature temperature
					    :top-p top-p
					    :frequency-penalty frequency-penalty
					    :presence-penalty presence-penalty
                                            :stream nil)))

    ;; (message "REQUEST %s" url-request-data)

    (setq org-ai--current-request-buffer
          (url-retrieve
           endpoint
           (lambda (_events)
             (unless (org-ai--maybe-show-openai-request-error
                      org-ai--current-request-buffer)
               (when callback
                 (with-current-buffer org-ai--current-request-buffer
                   (condition-case err
                       (progn (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                                (goto-char url-http-end-of-headers))
                              (if-let* ((result (json-read))
                                        (usage (alist-get 'usage result))
                                        (choices (alist-get 'choices result))
                                        (choice (aref choices 0))
                                        (message (alist-get 'message choice))
                                        (role (alist-get 'role message))
                                        (content (alist-get 'content message)))
                                  (funcall callback content role usage)
                                (funcall callback nil nil nil)))
                     (error (org-ai--show-error err)))))))))

    ;;(display-buffer-use-some-window org-ai--current-request-buffer nil)

    org-ai--current-request-buffer))

;; (org-ai-chat-request
;;  :messages (org-ai--collect-chat-messages "Hello, how are you?")
;;  :model "gpt-4"
;;  :callback (lambda (content role usage)
;;              (message "content: %s" content)
;;              (message "ROLE: %s" role)
;;              (message "USAGE: %s" usage)))

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
          (insert "Error from OpenAI API:\n\n")
          (insert error-message)
          (display-buffer buf)
          (goto-char (point-min))
          (toggle-truncate-lines -1)
          (read-only-mode 1)
          ;; close buffer when q is pressed
          (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer)))
          t))
    (error nil)))

(cl-defun org-ai--payload (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty stream)
  "Create the payload for the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `MODEL' is the model to use. `MAX-TOKENS' is the
maximum number of tokens to generate. `TEMPERATURE' is the
temperature of the distribution. `TOP-P' is the top-p value.
`FREQUENCY-PENALTY' is the frequency penalty. `PRESENCE-PENALTY'
is the presence penalty.
`STREAM' is a boolean indicating whether to stream the response."
  (let* ((input (if messages `(messages . ,messages) `(prompt . ,prompt)))
         ;; TODO yet unsupported properties: n, stop, logit_bias, user
         (data (map-filter (lambda (x _) x)
                           `(,input
                             (model . ,model)
                             ,@(when stream            `((stream . ,stream)))
                             ,@(when max-tokens        `((max_tokens . ,max-tokens)))
                             ,@(when temperature       `((temperature . ,temperature)))
                             ,@(when top-p             `((top_p . ,top-p)))
                             ,@(when frequency-penalty `((frequency_penalty . ,frequency-penalty)))
                             ,@(when presence-penalty  `((presence_penalty . ,presence-penalty)))))))
    (encode-coding-string (json-encode data) 'utf-8)))

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

            (while (and (not errored) (search-forward "data: " nil t))
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
                       (goto-char org-ai--url-buffer-last-position-marker)))))))))))))

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
  (setq org-ai--current-chat-role nil))

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
