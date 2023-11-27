;;; org-ai-oobabooga.el --- Integration of oobabooga/text-generation-webui  -*- lexical-binding: t; -*-

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

;; This integrates org-ai with the API provided by
;; https://github.com/oobabooga/text-generation-webui to support locally run
;; text generation models.

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'json)
(require 'org)
(require 'org-element)
(require 'websocket)
(require 'org-ai-block)


(defcustom org-ai-oobabooga-websocket-url "ws://127.0.0.1:5005/api/v1/stream"
  "URL to use for the websocket connection.
See https://github.com/oobabooga/text-generation-webui#api."
  :type 'string
  :group 'org-ai-oobabooga)

(defcustom org-ai-oobabooga-system-prefix "PROMPT: "
  "Prefix to use for system messages when assembling the full prompt."
  :type 'string
  :group 'org-ai-oobabooga)

(defcustom org-ai-oobabooga-user-prefix "You: "
  "Prefix to use for user messages when assembling the full prompt."
  :type 'string
  :group 'org-ai-oobabooga)

(defcustom org-ai-oobabooga-assistant-prefix "Assistant: "
  "Prefix to use for AI assistant messages when assembling the full prompt."
  :type 'string
  :group 'org-ai-oobabooga)

(defcustom org-ai-oobabooga-default-system-prompt nil
  "Default system prompt to use if none is specified.
If nil, no system prompt is used."
  :type 'string
  :group 'org-ai-oobabooga)

(defcustom org-ai-oobabooga-stopping-strings (list org-ai-oobabooga-user-prefix)
  "List of strings that will cause the stream to be stopped."
  :type 'string
  :group 'org-ai-oobabooga)

(defcustom org-ai-oobabooga-default-payload nil
  "Default payload to add to all requests.
E.g. if you want to set the temperature and top_p values:
  (setq org-ai-oobabooga-default-payload
        '((:temperature . 0.7) (:top_p . 0.1)))"
  :type 'alist
  :group 'org-ai-oobabooga)

(defvar org-ai-oobabooga--current-request nil
  "Holds the websocket while a request is in progress.")

(defvar org-ai-oobabooga--chat-got-first-response nil)
(make-variable-buffer-local 'org-ai-oobabooga--chat-got-first-response)

(defvar org-ai-oobabooga--chat-ai-role-inserted nil)
(make-variable-buffer-local 'org-ai-oobabooga--chat-ai-role-inserted)

(defun org-ai--merge-alist (a &rest bs)
  "Merge the alists A and BS.
If a key exists in multiple alists, the value from the last alist
is used."
  (let ((result (copy-alist a)))
    (cl-loop for b in bs
             do (cl-loop for (k . v) in b
                         do (if (assoc k result)
                                (setcdr (assoc k result) v)
                              (push (cons k v) result)))
             finally return result)))

(defun org-ai-oobabooga-create-prompt-default (messages)
  "Create a prompt from MESSAGES.
MESSAGES is a vector of (:role role :content content) plists.
:role can be 'system, 'user or 'assistant."
  (let ((system-prefix org-ai-oobabooga-system-prefix)
        (user-prefix org-ai-oobabooga-user-prefix)
        (assistant-prefix org-ai-oobabooga-assistant-prefix))
    (org-ai--stringify-chat-messages messages
                                     :default-system-prompt org-ai-oobabooga-default-system-prompt
                                     :user-prefix user-prefix
                                     :assistant-prefix assistant-prefix
                                     :system-prefix system-prefix)))

(defcustom org-ai-oobabooga-create-prompt-function 'org-ai-oobabooga-create-prompt-default
  "Function to create the prompt that will be passed to the LLM.
MESSAGES is a vector of (:role role :content content) plists.
:role can be 'system, 'user or 'assistant.
Example:
  (setq org-ai-oobabooga-create-prompt-function
        (lambda (messages)
          (cl-loop for msg across messages
                   for role = (plist-get msg :role)
                   for content = (plist-get msg :content)
                   collect (cond ((eql role 'system) (format \"SYSTEM %s\" content))
                                 ((eql role 'user) (format \"USER %s\" content))
                                 ((eql role 'assistant) (format \"ASSISTANT %s\" content)))
                   into result
                   finally return (string-join result \"\n\n\"))))"
  :type 'function
  :group 'org-ai-oobabooga)

(cl-defun org-ai-oobabooga-stream (&optional &key
                                             messages context
                                             max-tokens temperature top-p)
  "Send a request to the server from within an org-ai block.
MESSAGES is a list of messages to send to the server.
CONTEXT is the context of the block that is being processed.
MAX-TOKENS is the max-tokens value to use.
TEMPERATURE is the temperature value to use.
TOP-P is the top-p value to use."
  (let ((context (or context (org-ai-special-block)))
        (buffer (current-buffer)))
    (let* ((info (org-ai-get-block-info context))
           (max-tokens (or max-tokens (alist-get :max-tokens info)))
           (temperature (or temperature (alist-get :temperature info)))
           (top-p (or top-p (alist-get :top-p info)))
           (callback (lambda (result) (org-ai-oobabooga--insert-chat-completion-response
                                       context buffer result)))
           (prompt (funcall org-ai-oobabooga-create-prompt-function messages)))
      (org-ai-oobabooga-stream-request :prompt prompt
                                       :callback callback
                                       :max-tokens max-tokens
                                       :temperature temperature
                                       :top-p top-p))))

(cl-defun org-ai-oobabooga-stream-request (&optional &key
                                                     prompt callback
                                                     max-tokens temperature top-p)
  "Send a request to the oobabooga/text-generation-webui server.
PROMPT is the prompt to send.
CALLBACK is the function to call when a response is received.
MAX-TOKENS is the max-tokens value to use.
TEMPERATURE is the temperature value to use.
TOP-P is the top-p value to use."
  (let ((msg (let ((json-object-type 'alist)
                   (payload `((:prompt . ,prompt)
                              (:stopping_strings . ,org-ai-oobabooga-stopping-strings))))
               (when max-tokens (push `(:max_new_tokens . ,max-tokens) payload))
               (when temperature (push `(:temperature . ,temperature) payload))
               (when top-p (push `(:top_p . ,top-p) payload))
               (when org-ai-oobabooga-default-payload
                 (setq payload (org-ai--merge-alist org-ai-oobabooga-default-payload payload)))
               (json-encode payload)))
        (ws (websocket-open org-ai-oobabooga-websocket-url
                            :on-message (lambda (_ws frame)
                                          (funcall callback (websocket-frame-text frame)))
                            :on-close (lambda (_ws)
                                        (setq org-ai-oobabooga--current-request nil)
                                        (funcall callback nil)))))
    (setq org-ai--current-insert-position-marker nil
          org-ai-oobabooga--chat-got-first-response nil
          org-ai-oobabooga--chat-ai-role-inserted nil
          org-ai-oobabooga--current-request ws)

    ;; (message "[org-ai-oobabooga] SENDING %s" msg)

    (websocket-send-text ws msg)))

(defun org-ai-oobabooga-stop ()
  "Stop the current request."
  (interactive)
  (when org-ai-oobabooga--current-request
    (websocket-close org-ai-oobabooga--current-request)))

(defun org-ai-oobabooga--read-response (response)
  "Parse RESPONSE as json."
  (let ((json-object-type 'alist)
        (json-key-type 'keyword)
        (json-array-type 'vector))
    (condition-case _err
        (json-read-from-string response)
      (error
       '((:error . (format "Failed to parse response: %s" response)))))))

(defun org-ai-oobabooga--insert-chat-completion-response (context buffer response)
  "Handle stream response from oobabooga/text-generation-webui.
CONTEXT is the context of the block that was being processed.
BUFFER is the buffer that the block is in.
RESPONSE is the parsed json response from the server."

  ;; (message "[org-ai-oobabooga] RECEIVED %s" response)

  (if response
      ;; process response
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
            (let* ((response (org-ai-oobabooga--read-response response))
                   (err (alist-get :error response))
                   (type (alist-get :event response))
                   (text (alist-get :text response)))
              (when err (error err))

              (when (not org-ai-oobabooga--chat-ai-role-inserted)
                (insert "\n[AI]: ")
                (run-hook-with-args 'org-ai-after-chat-insertion-hook 'role "assistant")
                (setq org-ai-oobabooga--chat-ai-role-inserted t))

              (cond
               ((string= type "text_stream")
                (when (or org-ai-oobabooga--chat-got-first-response
                          (not (string= (string-trim text) "")))
                  (when (and (not org-ai-oobabooga--chat-got-first-response)
                             (string-prefix-p "```" text))
                    ;; start markdown codeblock responses on their own line
                    (insert "\n"))
                  (let ((text (if org-ai-oobabooga--chat-got-first-response
                                  text
                                (string-trim-left text))))
                    (insert (decode-coding-string text 'utf-8))
                    (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text))
                  (setq org-ai-oobabooga--chat-got-first-response t)))

               ((string= type "stream_end")
                (let ((text "\n\n[ME]: "))
                  (insert text)
                  (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text))))

              (setq org-ai--current-insert-position-marker (point-marker))))))

    ;; insert new prompt and change position
    (with-current-buffer buffer
      (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end "")
      (org-element-cache-reset))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-oobabooga)

;;; org-ai-oobabooga.el ends here
