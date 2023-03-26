;;; org-ai-openai.el --- OpenAI related functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Robert Krahn

;; Author: Robert Krahn <robert@kra.hn>
;; URL: https://github.com/rksm/org-ai
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.2"))

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

(defcustom org-ai-openai-api-token nil
  "This is your OpenAI API token that you need to specify. You can retrieve it at https://platform.openai.com/account/api-keys."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-use-auth-source t
  "If non-nil, use auth-source to retrieve the OpenAI API token.
The secret should be stored in the format
  machine api.openai.com login org-ai password <your token>
in the `auth-sources' file."
  :type 'boolean
  :group 'org-ai)

(when (and org-ai-use-auth-source (not org-ai-openai-api-token))
  (require 'auth-source)
  (when-let ((secret (auth-source-pick-first-password :host "api.openai.com" :user "org-ai")))
    (setq org-ai-openai-api-token secret)))

(defcustom org-ai-default-completion-model "text-davinci-003"
  "The default model to use for completion requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-chat-model "gpt-3.5-turbo"
  "The default model to use for chat-gpt requests. See https://platform.openai.com/docs/models for other options."
  :type 'string
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

(defvar org-ai-openai-chat-endpoint "https://api.openai.com/v1/chat/completions")

(defvar org-ai-openai-completion-endpoint "https://api.openai.com/v1/completions")

(defvar org-ai--current-request-buffer nil
  "Internal var that stores the current request buffer.")

(defvar org-ai--current-request-callback nil
  "Internal var that stores the current request callback.")

(defvar org-ai--current-insert-position nil
  "Where to insert the result.")
(make-variable-buffer-local 'org-ai--current-insert-position)

(defvar org-ai--current-chat-role nil
  "During chat response streaming, this holds the role of the \"current speaker\".")

(defvar org-ai--chat-got-first-response nil)
(make-variable-buffer-local 'org-ai--chat-got-first-response)

(defvar org-ai--url-buffer-last-position nil
  "Local buffer var to store last read position.")
;; (make-variable-buffer-local 'org-ai--url-buffer-last-position)
;; (makunbound 'org-ai--url-buffer-last-position)

(defvar org-ai--debug-data nil)
(defvar org-ai--debug-data-raw nil)

;; (with-current-buffer "*scratch*"
;;   (erase-buffer)
;;   (pop-to-buffer "*scratch*" t)
;;   (let ((n 16))
;;    (insert (car (nth n org-ai--debug-data-raw)))
;;    (goto-char (cadr (nth n org-ai--debug-data-raw)))
;;    (beginning-of-line)))

(defun org-ai-keyboard-quit ()
  "If there is currently a running request, cancel it."
  (interactive)
  (condition-case _
      (when org-ai--current-request-buffer
        (org-ai-interrupt-current-request))
    (error nil)))

(defun org-ai--install-keyboard-quit-advice ()
  "Cancel current request when `keyboard-quit' is called."
  (unless (advice-member-p #'org-ai-keyboard-quit 'keyboard-quit)
    (advice-add 'keyboard-quit :before #'org-ai-keyboard-quit)))

(defun org-ai--uninstall-keyboard-quit-advice ()
  "Remove the advice that cancels current request when `keyboard-quit' is called."
  (advice-remove 'keyboard-quit #'org-ai-keyboard-quit))

(defun org-ai-ctrl-c-ctrl-c ()
  "This is added to `org-ctrl-c-ctrl-c-hook' to enable the `org-mode' integration."
  (when-let ((context (org-ai-special-block)))
    (org-ai-complete-block)
    t))

(defvar yas-snippet-dirs)

(defun org-ai-install-yasnippets ()
  "Installs org-ai snippets."
  (interactive)
  (let ((snippet-dir (expand-file-name "snippets/"
                                       (file-name-directory (locate-library "org-ai")))))
    (unless (boundp 'yas-snippet-dirs)
      (setq yas-snippet-dirs nil))
    (add-to-list 'yas-snippet-dirs snippet-dir t)
    (when (fboundp 'yas-load-directory)
      (yas-load-directory snippet-dir))))

(defun org-ai-special-block (&optional el)
  "Are we inside a #+begin_ai...#+end_ai block? `EL' is the current special block."
  (let (org-element-use-cache) ;; with cache enabled we get weird Cached element is incorrect warnings
    (let ((context (org-element-context el)))
      (if (equal 'special-block (org-element-type context))
          context
        (when-let ((parent (org-element-property :parent context)))
          (org-ai-special-block parent))))))

(defun org-ai-get-block-info (&optional context)
  "Parse the header of #+begin_ai...#+end_ai block.
`CONTEXT' is the context of the special block. Return an alist of
key-value pairs."
  (let* ((context (or context (org-ai-special-block)))
         (header-start (org-element-property :post-affiliated context))
         (header-end (org-element-property :contents-begin context))
         (string (string-trim (buffer-substring-no-properties header-start header-end)))
         (string (string-trim-left (replace-regexp-in-string "^#\\+begin_ai" "" string))))
    (org-babel-parse-header-arguments string)))

(defun org-ai-get-block-content (&optional context)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
`CONTEXT' is the context of the special block."
  (let* ((context (or context (org-ai-special-block)))
         (content-start (org-element-property :contents-begin context))
         (content-end (org-element-property :contents-end context)))
    (string-trim (buffer-substring-no-properties content-start content-end))))

(defun org-ai--request-type (info)
  "Look at the header of the #+begin_ai...#+end_ai block.
returns the type of request. `INFO' is the alist of key-value
pairs from `org-ai-get-block-info'."
  (cond
   ((not (eql 'x (alist-get :chat info 'x))) 'chat)
   ((not (eql 'x (alist-get :completion info 'x))) 'completion)
   ((not (eql 'x (alist-get :image info 'x))) 'image)
   (t 'chat)))

(defun org-ai-complete-block ()
  "Main command which is normally bound to \\[org-ai-complete-block].
When you are inside an #+begin_ai...#+end_ai block, it will send
the text content to the OpenAI API and replace the block with the
result."
  (interactive)
  (let* ((context (org-ai-special-block))
         (content (org-ai-get-block-content context))
         (info (org-ai-get-block-info context))
         (req-type (org-ai--request-type info))
         (sys-prompt-for-all-messages (or (not (eql 'x (alist-get :sys-everywhere info 'x)))
                                          org-ai-default-inject-sys-prompt-for-all-messages)))
    (cl-case req-type
      (completion (org-ai-stream-completion :prompt (encode-coding-string content 'utf-8)
                                            :context context))
      (image (org-ai-create-and-embed-image context))
      (t (org-ai-stream-completion :messages (org-ai--collect-chat-messages content sys-prompt-for-all-messages)
                                   :context context)))))

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
      (setq org-ai--current-insert-position nil)
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
              (unless org-ai--current-insert-position
                (push-mark (org-element-property :contents-end context)))
              (let ((pos (or org-ai--current-insert-position (org-element-property :contents-end context))))
                (save-excursion
                  (goto-char pos)

                  (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                    (insert "\n")
                    (backward-char))
                  (insert text)
                  (setq org-ai--current-insert-position (point)))))))))

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
          (let ((pos (or org-ai--current-insert-position (org-element-property :contents-end context))))
            (save-excursion
              (goto-char pos)

              ;; make sure we have enough space at end of block, don't write on same line
              (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                (insert "\n")
                (backward-char))

              ;; insert text
              (if-let* ((choices (or (alist-get 'choices response)
                                     (plist-get response 'choices)))
                        (choice (aref choices 0))
                        (delta (plist-get choice 'delta)))
                  (cond
                   ((plist-get delta 'content)
                    (let ((text (plist-get delta 'content)))
                      (when (or org-ai--chat-got-first-response (not (string= (string-trim text) "")))
                        (insert (decode-coding-string text 'utf-8)))
                      (setq org-ai--chat-got-first-response t)))
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
                          (insert "\n[SYS]: "))))))))

              (setq org-ai--current-insert-position (point))))))

    ;; insert new prompt and change position
    (with-current-buffer buffer
      (goto-char org-ai--current-insert-position)
      (insert "\n\n[ME]: "))))

(cl-defun org-ai-stream-request (&optional &key prompt messages callback model max-tokens temperature top-p frequency-penalty presence-penalty)
  "Send a request to the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `CALLBACK' is the callback function. `MODEL' is the
model to use. `MAX-TOKENS' is the maximum number of tokens to
generate. `TEMPERATURE' is the temperature of the distribution.
`TOP-P' is the top-p value. `FREQUENCY-PENALTY' is the frequency
penalty. `PRESENCE-PENALTY' is the presence penalty."
  (unless org-ai-openai-api-token
    (error "Please set `org-ai-openai-api-token' to your OpenAI API token"))
  (let* ((token org-ai-openai-api-token)
         (url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,token) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (endpoint (if messages org-ai-openai-chat-endpoint org-ai-openai-completion-endpoint))
         (url-request-data (encode-coding-string (org-ai--payload :prompt prompt
								  :messages messages
								  :model model
								  :max-tokens max-tokens
								  :temperature temperature
								  :top-p top-p
								  :frequency-penalty frequency-penalty
								  :presence-penalty presence-penalty) 
                                                 'utf-8)))

    ;; (message "REQUEST %s" url-request-data)

    (setq org-ai--current-request-callback callback)

    (setq org-ai--current-request-buffer
          (url-retrieve
           endpoint
           (lambda (_events)
             (org-ai-reset-stream-state))))

    ;; (pop-to-buffer org-ai--current-request-buffer)

    (unless (member 'org-ai--url-request-on-change-function after-change-functions)
      (with-current-buffer org-ai--current-request-buffer
        (add-hook 'after-change-functions #'org-ai--url-request-on-change-function nil t)))))

(cl-defun org-ai--payload (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty)
  "Create the payload for the OpenAI API.
`PROMPT' is the query for completions `MESSAGES' is the query for
chatgpt. `MODEL' is the model to use. `MAX-TOKENS' is the
maximum number of tokens to generate. `TEMPERATURE' is the
temperature of the distribution. `TOP-P' is the top-p value.
`FREQUENCY-PENALTY' is the frequency penalty. `PRESENCE-PENALTY'
is the presence penalty."
  (let* ((input (if messages `(messages . ,messages) `(prompt . ,prompt)))
         ;; TODO yet unsupported properties: n, stop, logit_bias, user
         (data (map-filter (lambda (x _) x)
                           `(,input
                             (model . ,model)
                             (stream . t)
                             ,@(when max-tokens        `((max_tokens . ,max-tokens)))
                             ,@(when temperature       `((temperature . ,temperature)))
                             ,@(when top-p             `((top_p . ,top-p)))
                             ,@(when frequency-penalty `((frequency_penalty . ,frequency-penalty)))
                             ,@(when presence-penalty  `((presence_penalty . ,presence-penalty)))))))
    (json-encode data)))

(defun org-ai--url-request-on-change-function (_beg _end _len)
  "Look into the url-request buffer and manually extracts JSON stream responses.
Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range."
  (with-current-buffer org-ai--current-request-buffer
    (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
      (save-excursion
        (if org-ai--url-buffer-last-position
            (goto-char org-ai--url-buffer-last-position)
          (goto-char url-http-end-of-headers)
          (setq org-ai--url-buffer-last-position (point)))

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
                    (setq org-ai--url-buffer-last-position (point))
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
                        (setq org-ai--url-buffer-last-position (point)))
                    (error
                     (setq errored t)
                     (goto-char org-ai--url-buffer-last-position))))))))))))

(defun org-ai-interrupt-current-request ()
  "Interrupt the current request."
  (interactive)
  (when (and org-ai--current-request-buffer (buffer-live-p org-ai--current-request-buffer))
    (let (kill-buffer-query-functions)
      (kill-buffer org-ai--current-request-buffer))
    (setq org-ai--current-request-buffer nil)
    (org-ai-reset-stream-state)))

(defun org-ai-reset-stream-state ()
  "Reset the stream state."
  (interactive)
  (when (and org-ai--current-request-buffer (buffer-live-p org-ai--current-request-buffer))
    (with-current-buffer org-ai--current-request-buffer
      (remove-hook 'after-change-functions #'org-ai--url-request-on-change-function t)
      (setq org-ai--url-buffer-last-position nil)))
  (setq org-ai--current-request-callback nil)
  (setq org-ai--url-buffer-last-position nil)
  (setq org-ai--current-chat-role nil))

(defun org-ai--collect-chat-messages (content-string &optional persistant-sys-prompts)
  "Takes `CONTENT-STRING' and splits it by [SYS]:, [ME]: and [AI]: markers.
If `PERSISTANT-SYS-PROMPTS' is non-nil, [SYS] prompts are
intercalated. The [SYS] prompt used is either
`org-ai-default-chat-system-prompt' or the first [SYS] prompt
found in `CONTENT-STRING'."
  (with-temp-buffer
    (erase-buffer)
    (insert content-string)
    (goto-char (point-min))

    (let* (;; collect all positions before [ME]: and [AI]:
           (sections (cl-loop while (search-forward-regexp "\\[SYS\\]:\\|\\[ME\\]:\\|\\[AI\\]:" nil t)
                              collect (save-excursion
                                        (goto-char (match-beginning 0))
                                        (point))))

           ;; make sure we have from the beginning if there is no first marker
           (sections (if (not sections)
                         (list (point-min))
                       (if (not (= (car sections) (point-min)))
                           (cons (point-min) sections)
                         sections)))

           (parts (cl-loop for (start end) on sections by #'cdr
                           collect (string-trim (buffer-substring-no-properties start (or end (point-max))))))

           ;; if no role is specified, assume [ME]
           (parts (if (and
                       (not (string-prefix-p "[SYS]:" (car parts)))
                       (not (string-prefix-p "[ME]:" (car parts)))
                       (not (string-prefix-p "[AI]:" (car parts))))
                      (progn (setf (car parts) (concat "[ME]: " (car parts)))
                             parts)
                    parts))

           ;; create (:role :content) list
           (messages (cl-loop for part in parts
                              collect (cl-destructuring-bind (type &rest content) (split-string part ":")
                                        (let ((type (string-trim type))
                                              (content (string-trim (string-join content ":"))))
                                          (list :role (cond ((string= type "[SYS]") 'system)
                                                            ((string= type "[ME]") 'user)
                                                            ((string= type "[AI]") 'assistant)
                                                            (t 'assistant))
                                                :content content)))))

           (messages (cl-remove-if-not (lambda (x) (not (string-empty-p (plist-get x :content)))) messages))

           ;; merge messages with same role
           (messages (cl-loop with last-role = nil
                              with result = nil
                              for (_ role _ content) in messages
                              if (eql role last-role)
                              do (let ((last (pop result)))
                                   (push (list :role role :content (string-join (list (plist-get last :content) content) "\n")) result))
                              else
                              do (push (list :role role :content content) result)
                              do (setq last-role role)
                              finally return (reverse result)))

           (starts-with-sys-prompt-p (and messages (eql (plist-get (car messages) :role) 'system)))

           (sys-prompt (if starts-with-sys-prompt-p
                           (plist-get (car messages) :content)
                         org-ai-default-chat-system-prompt))

           (messages (if persistant-sys-prompts
                         (cl-loop with result = nil
                                  for (_ role _ content) in messages
                                  if (eql role 'assistant)
                                  do (push (list :role 'assistant :content content) result)
                                  else if (eql role 'user)
                                  do (progn
                                       (push (list :role 'system :content sys-prompt) result)
                                       (push (list :role 'user :content content) result))
                                  finally return (reverse result))
                       messages)))

      (apply #'vector messages))))

;; deal with unspecified prefix
(cl-assert
 (equal
  (let ((test-string "\ntesting\n  [ME]: foo bar baz zorrk\nfoo\n[AI]: hello hello[ME]: "))
    (org-ai--collect-chat-messages test-string))
  '[(:role user :content "testing\nfoo bar baz zorrk\nfoo")
    (:role assistant :content "hello hello")]))

;; sys prompt
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant"))
    (org-ai--collect-chat-messages test-string))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")]))

;; sys prompt intercalated
(cl-assert
 (equal
  (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
    (org-ai--collect-chat-messages test-string t))
  '[(:role system :content "system")
    (:role user :content "user")
    (:role assistant :content "assistant")
    (:role system :content "system")
    (:role user :content "user")]))

;; merge messages with same role
(cl-assert
 (equal
  (let ((test-string "[ME]: hello [ME]: world")) (org-ai--collect-chat-messages test-string))
  '[(:role user :content "hello\nworld")]))

;; (comment
;;   (with-current-buffer "org-ai-mode-test.org"
;;    (org-ai--collect-chat-messages (org-ai-get-block-content))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-openai)

;;; org-ai-openai.el ends here

