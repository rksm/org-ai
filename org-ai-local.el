;;; org-ai-local.el --- Integration of oobabooga/text-generation-webui  -*- lexical-binding: t; -*-

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


(defcustom org-ai-local-system-prefix "PROMPT: "
  "Prefix to use for system messages when assembling the full prompt."
  :type 'string
  :group 'org-ai-local)

(defcustom org-ai-local-user-prefix "You: "
  "Prefix to use for user messages when assembling the full prompt."
  :type 'string
  :group 'org-ai-local)

(defcustom org-ai-local-assistant-prefix "Assistant: "
  "Prefix to use for AI assistant messages when assembling the full prompt."
  :type 'string
  :group 'org-ai-local)

(defcustom org-ai-local-default-system-prompt nil
  "Default system prompt to use if none is specified. If nil, no
system prompt is used."
  :type 'string
  :group 'org-ai-local)

(defcustom org-ai-local-stopping-strings nil
  "List of strings that, if encountered in the response, will
cause the stream to be stopped."
  :type 'string
  :group 'org-ai-local)

(defcustom org-ai-local-websocket-url "ws://127.0.0.1:5005/api/v1/stream"
  "URL to use for the websocket connection. See
https://github.com/oobabooga/text-generation-webui#api."
  :type 'string
  :group 'org-ai-local)

(defvar org-ai-local--current-request nil
  "While a request is being processed, this variable holds the
websocket for it.")

(defvar org-ai-local--chat-got-first-response nil)
(make-variable-buffer-local 'org-ai-local--chat-got-first-response)

(defvar org-ai-local--chat-ai-role-inserted nil)
(make-variable-buffer-local 'org-ai-local--chat-ai-role-inserted)

(defun org-ai--merge-alist (a &rest bs)
  ""
  (let ((result (copy-alist a)))
    (cl-loop for b in bs
             do (cl-loop for (k . v) in b
                         do (if (assoc k result)
                                (setcdr (assoc k result) v)
                              (add-to-list 'result (cons k v))))
             finally return result)))

(cl-defun org-ai-local-stream (&optional &key messages context)
  (let ((context (or context (org-ai-special-block)))
        (buffer (current-buffer))
        (system-prefix org-ai-local-system-prefix)
        (user-prefix org-ai-local-user-prefix)
        (assistant-prefix org-ai-local-assistant-prefix))
    (let* ((info (org-ai-get-block-info context))
           (callback (lambda (result) (org-ai-local--insert-chat-completion-response
                                       context buffer result)))
           (prompt (org-ai--stringify-chat-messages messages
                                                    :default-system-prompt org-ai-local-default-system-prompt
                                                    :user-prefix user-prefix
                                                    :assistant-prefix assistant-prefix
                                                    :system-prefix system-prefix)))
      (org-ai-local-stream-request :prompt prompt
                                   :callback callback))))

(cl-defun org-ai-local-stream-request (&optional &key prompt callback)
  (let ((msg (let ((json-object-type 'alist))
               (json-encode `((:prompt . ,prompt)
                              (:max_new_tokens . 250)
                              (:stopping_strings . ,org-ai-local-stopping-strings)))))
        (ws (websocket-open org-ai-local-websocket-url
                            :on-message (lambda (_ws frame)
                                          (funcall callback (websocket-frame-text frame)))
                            :on-close (lambda (_ws)
                                        (setq org-ai-local--current-request nil)
                                        (funcall callback nil)))))
    (setq org-ai--current-insert-position-marker nil
          org-ai-local--chat-got-first-response nil
          org-ai-local--chat-ai-role-inserted nil
          org-ai-local--current-request ws)
    (message "[org-ai-local] SENDING %s" msg)
    (websocket-send-text ws msg)))

(defun org-ai-local-stop ()
  ""
  (interactive)
  (when org-ai-local--current-request
    (websocket-close org-ai-local--current-request)))

(defun org-ai-local--read-response (response)
  ""
  (let ((json-object-type 'alist)
        (json-key-type 'keyword)
        (json-array-type 'vector))
    (condition-case _err
        (json-read-from-string response)
      (error
       '((:error . (format "Failed to parse response: %s" response)))))))

(defun org-ai-local--insert-chat-completion-response (context buffer response)
  ""
  (message "[org-ai-local] RECEIVED %s" response)
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
            (let* ((response (org-ai-local--read-response response))
                   (err (alist-get :error response))
                   (type (alist-get :event response))
                   (text (alist-get :text response)))
              (when err (error err))

              (when (not org-ai-local--chat-ai-role-inserted)
                (insert "\n[AI]: ")
                (run-hook-with-args 'org-ai-after-chat-insertion-hook 'role "assistant")
                (setq org-ai-local--chat-ai-role-inserted t))

              (cond
               ((string= type "text_stream")
                (when (or org-ai-local--chat-got-first-response
                          (not (string= (string-trim text) "")))
                  (when (and (not org-ai-local--chat-got-first-response)
                             (string-prefix-p "```" text))
                    ;; start markdown codeblock responses on their own line
                    (insert "\n"))
                  (let ((text (if org-ai-local--chat-got-first-response
                                  text
                                (string-trim-left text))))
                    (insert (decode-coding-string text 'utf-8))
                    (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text))
                  (setq org-ai-local--chat-got-first-response t)))

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

(provide 'org-ai-local)

;;; org-ai-local.el ends here
