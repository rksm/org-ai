;;; org-ai.el --- Emacs org-mode integration for the OpenAI API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Robert Krahn

;; Author: Robert Krahn <robert@kra.hn>
;; URL: https://github.com/rksm/org-ai
;; Package-Requires: ((emacs "28.2"))
;; Version: 0.1.1

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

;; Provides a minor-mode for org-mode that allows you to interact with the
;; OpenAI API. You need an OpenAI API account and an API key for that. See
;; https://platform.openai.com/account/api-keys.

;;; Code:

(defcustom org-ai-openai-api-token nil
  "Your OpenAI API token. You can retrieve it at
https://platform.openai.com/account/api-keys."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-completion-model "text-davinci-003"
  "The default model to use for completion requests. See
https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-chat-model "gpt-3.5-turbo"
  "The default model to use for chat-gpt requests. See
https://platform.openai.com/docs/models for other options."
  :type 'string
  :group 'org-ai)

(defcustom org-ai-default-max-tokens 2000
  "The default maximum number of tokens to generate. This is what
costs money."
  :type 'string
  :group 'org-ai)

(defvar org-ai-openai-chat-endpoint "https://api.openai.com/v1/chat/completions"
  "")

(defvar org-ai-openai-completion-endpoint "https://api.openai.com/v1/completions"
  "")

;; define org-ai-mode-map
(defvar org-ai-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-a") 'org-ai)
    map)
  "Keymap for org-ai-mode.")

;; create a minor-mode for org-mode
(define-minor-mode org-ai-mode
  "Toggle org-ai-mode."
        :init-value nil
        :lighter "org-ai"
        :keymap org-ai-mode-map
        :group 'org-ai
        (add-hook 'org-ctrl-c-ctrl-c-hook 'org-ai-ctrl-c-ctrl-c nil t))

(defun org-ai-ctrl-c-ctrl-c ()
  "org-mode integration."
  (when-let ((context (org-ai-special-block)))
    (org-ai-complete-block)
    t))

(defun org-ai-special-block (&optional el)
  "Are inside a #+begin_ai...#+end_ai block?"
  (let ((context (org-element-context el)))
      (if (equal 'special-block (org-element-type context))
          context
        (when-let ((parent (org-element-property :parent context)))
          (message "parent %s" parent)
          (org-ai-special-block parent)))))

(defun org-ai-get-block-info (&optional context)
  "Parse header of #+begin_ai...#+end_ai block. Returns an alist of key-value pairs."
  (let* ((context (or context (org-ai-special-block)))
         (header-start (org-element-property :post-affiliated context))
         (header-end (org-element-property :contents-begin context))
         (string (string-trim (buffer-substring-no-properties header-start header-end)))
         (string (string-trim-left (replace-regexp-in-string "^#\\+begin_ai" "" string))))
    (org-babel-parse-header-arguments string)))

(defun org-ai-get-block-content (&optional context)
  "Extracts the text content of the #+begin_ai...#+end_ai block."
  (let* ((context (or context (org-ai-special-block)))
         (content-start (org-element-property :contents-begin context))
         (content-end (org-element-property :contents-end context)))
    (string-trim (buffer-substring-no-properties content-start content-end))))


(defun org-ai--wants-to-chat (info)
  ""
  (not (eql 'x (alist-get :chat info 'x))))

(defun org-ai-complete-block ()
  "Main command, normally bound to C-c C-c. When you are inside an
#+begin_ai...#+end_ai block, it will send the text content to the
OpenAI API and replace the block with the result."
  (interactive)
  (let* ((context (org-ai-special-block))
         (content (org-ai-get-block-content context))
         (chat (org-ai--wants-to-chat (org-ai-get-block-info context))))
    (if chat
        (org-ai-stream-completion :messages (org-ai--collect-chat-messages content) :context context)
      (org-ai-stream-completion :prompt (encode-coding-string prompt 'utf-8) :context context))))

(defvar org-ai--current-request-buffer nil
  "Internal var that stores the current request buffer.")

(defvar org-ai--current-request-callback nil
  "Internal var that stores the current request callback.")

(defvar org-ai--current-insert-position nil
  "Where to insert the result.")
(make-variable-buffer-local 'org-ai--current-insert-position)

(defvar org-ai--current-chat-role nil
  "During chat response streaming, this holds the role of the \"current speaker\".")

(defvar org-ai--url-buffer-last-position nil
  "Local buffer var to store last read position")
;; (make-variable-buffer-local 'org-ai--url-buffer-last-position)
;; (makunbound 'org-ai--url-buffer-last-position)

(cl-defun org-ai-stream-completion (&optional &key prompt messages model max-tokens temperature top-p frequency-penalty presence-penalty context)
  "Starts an server-sent event stream."
  (lexical-let* ((context (or context (org-ai-special-block)))
                 (buffer (current-buffer)))
    (let* ((content (org-ai-get-block-content context))
           (info (org-ai-get-block-info context))
           (model (or model (alist-get :model info) (if messages org-ai-default-chat-model org-ai-default-completion-model)))
           (max-tokens (or max-tokens (alist-get :max-tokens info) org-ai-default-max-tokens))
           (callback (if messages
                         (lambda (result) (org-ai--insert-chat-completion-response context buffer result))
                       (lambda (result) (org-ai--insert-stream-completion-response context buffer result)))))
      (setq org-ai--current-insert-position nil)
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
  "Response is one JSON message of the stream response."
  (if response
      (if-let ((error (plist-get response 'error)))
          (if-let ((message (plist-get error 'message))) (error message) (error error))
        (if-let* ((choice (aref (plist-get response 'choices) 0))
                  (text (plist-get choice 'text)))
            (with-current-buffer buffer
              (let ((pos (or org-ai--current-insert-position (org-element-property :contents-end context))))
                (save-excursion
                  (goto-char pos)
                  (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                    (insert "\n")
                    (backward-char))
                  (insert text)
                  (setq org-ai--current-insert-position (point)))))))))

(defun org-ai--insert-chat-completion-response (context buffer &optional response)
  "Response is one JSON message of the stream response. When `response' is nil, it means we are done."
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
                    (let* ((text (plist-get delta 'content))
                           ;; (text (decode-coding-string text 'utf-8))
                           )
                      (insert text)))
                   ((plist-get delta 'role)
                    (let ((role (plist-get delta 'role)))
                      (progn
                        (setq org-ai--current-chat-role role)
                        (if (or (string= role "assistant") (string= role "system"))
                            (insert "[AI]: ")
                          (insert "[ME]: ")))))))

              (setq org-ai--current-insert-position (point))))))

    ;; insert new prompt and change position
    (with-current-buffer buffer
      (goto-char org-ai--current-insert-position)
      (insert "\n[ME]: "))))

(cl-defun org-ai-stream-request (&optional &key prompt messages callback model max-tokens temperature top-p frequency-penalty presence-penalty)
  ""
  (let* ((token org-ai-openai-api-token)
         (url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,token) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (endpoint (if messages org-ai-openai-chat-endpoint org-ai-openai-completion-endpoint))
         (url-request-data (org-ai--payload :prompt prompt
                                            :messages messages
                                            :model model
                                            :max-tokens max-tokens
                                            :temperature temperature
                                            :top-p top-p
                                            :frequency-penalty frequency-penalty
                                            :presence-penalty presence-penalty)))

    ;; (message "REQUEST %s" url-request-data)

    (setq org-ai--current-request-callback callback)

    (setq org-ai--current-request-buffer
          (url-retrieve
           endpoint
           (lambda (events)
             (org-ai-reset-stream-state))))

    ;; (pop-to-buffer org-ai--current-request-buffer)

    (unless (member 'org-ai--url-request-on-change-function after-change-functions)
      (with-current-buffer org-ai--current-request-buffer
        (add-hook 'after-change-functions 'org-ai--url-request-on-change-function nil t)))))

(cl-defun org-ai--payload (&optional &key prompt messages callback model max-tokens temperature top-p frequency-penalty presence-penalty)
  ""
  (let* ((content (if messages `(messages . ,messages) `(prompt . ,prompt)))
         (data
          ;; TODO yet unsupported properties: n, stop, logit_bias, user
          (json-encode (map-filter (lambda (x _) x)
                                   `(,content
                                     (model . ,model)
                                     (stream . t)
                                     ,@(when max-tokens        `((max_tokens . ,max-tokens)))
                                     ,@(when temperature       `(temperature . ,temperature))
                                     ,@(when top-p             `(top_p . ,top-p))
                                     ,@(when frequency-penalty `(frequency_penalty . ,frequency-penalty))
                                     ,@(when presence-penalty  `(presence_penalty . ,presence-penalty)))))))
    data))

(defvar org-ai--debug-data nil "")
(defvar org-ai--debug-data-raw nil "")

;; (with-current-buffer "*scratch*"
;;   (erase-buffer)
;;   (pop-to-buffer "*scratch*" t)
;;   (let ((n 16))
;;    (insert (car (nth n org-ai--debug-data-raw)))
;;    (goto-char (cadr (nth n org-ai--debug-data-raw)))
;;    (beginning-of-line)))

(defun org-ai--url-request-on-change-function (beg end len)
  "Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range."
  (with-current-buffer org-ai--current-request-buffer
    (when url-http-end-of-headers
      (save-excursion
        (if org-ai--url-buffer-last-position
            (goto-char org-ai--url-buffer-last-position)
          (goto-char url-http-end-of-headers)
          (setq org-ai--url-buffer-last-position (point)))

        ;; Avoid a bug where we skip responses because url has modified the http
        ;; buffer and we are not where we think we are.
        ;; TODO this might break
        (unless (= (point) (line-end-position))
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
                  (condition-case err
                      (let ((data (json-read)))
                        (when org-ai--current-request-callback
                          (funcall org-ai--current-request-callback data))
                        (setq org-ai--url-buffer-last-position (point)))
                    (error
                     (setq errored t)
                     (goto-char org-ai--url-buffer-last-position))))))))))))

(defun org-ai-interrupt-current-request ()
  ""
  (interactive)
  (when (and org-ai--current-request-buffer (buffer-live-p org-ai--current-request-buffer))
    (kill-buffer org-ai--current-request-buffer)
    (org-ai-reset-stream-state)))

(defun org-ai-reset-stream-state ()
  ""
  (interactive)
  (when (and org-ai--current-request-buffer (buffer-live-p org-ai--current-request-buffer))
    (with-current-buffer org-ai--current-request-buffer
      (remove-hook 'after-change-functions 'org-ai--url-request-on-change-function t)
      (setq org-ai--url-buffer-last-position nil)))
  (setq org-ai--current-request-callback nil)
  (setq org-ai--url-buffer-last-position nil)
  (setq org-ai--current-chat-role nil))

(defun org-ai--collect-chat-messages (content-string)
  "Takes a string and splits it by [ME]: and [AI]: markers.
Will collect the text inbetween and produce a list of property
lists like '((:role user :content \"hello\") (:role system
:content \"world\")). If roles are repeated, will merge messages
together."
  (with-temp-buffer
   (erase-buffer)
   (insert content-string)
   (goto-char (point-min))
   (let* (;; collect all positions before [ME]: and [AI]:
          (sections (loop with last-pos = nil
                          while (search-forward-regexp "\\[ME\\]:\\|\\[AI\\]:" nil t)
                          collect (save-excursion
                                    (backward-char 5)
                                    (point))))
          ;; make sure we have from the beginning if there is no first marker
          (sections (if (not (= (car sections) (point-min)))
                        (cons (point-min) sections)
                      sections))
          (parts (loop for (start end) on sections by #'cdr
                       collect (string-trim (buffer-substring-no-properties start (or end (point-max))))))
          (parts (if (and
                      (not (string-suffix-p "[ME]:" (first parts)))
                      (not (string-suffix-p "[AI]:" (first parts))))
                     (progn (when (not (string-prefix-p "[ME]:" (first parts)))
                                (setf (first parts) (concat "[ME]: " (first parts))))
                            parts)
                   parts
                   ))

          ;; create (:role :content) list
          (messages (loop for part in parts
                          for (type content) = (split-string part ":")
                          for content = (string-trim content)
                          when (not (string-empty-p content))
                          collect (list :role (if (string= (string-trim type) "[ME]")
                                                  'user
                                                'system)
                                        :content (encode-coding-string content 'utf-8))))

          ;; merge messages with same role
          (messages (loop with last-role = nil
                          with result = nil
                          for (_ role _ content) in messages
                          if (eql role last-role)
                          do (let ((last (pop result)))
                               (push (list :role role :content (string-join (list (plist-get last :content) content) "\n")) result))
                          else
                          do (push (list :role role :content content) result)
                          do (setq last-role role)
                          finally return (reverse result))))
     (apply #'vector messages))))

(cl-assert
 (equal
  (let ((test-string "\ntesting\n  [ME]: foo bar baz zorrk\nfoo\n[AI]: hello hello[ME]: "))
    (org-ai--collect-chat-messages test-string))
  '[(:role user :content "testing\nfoo bar baz zorrk\nfoo") (:role system :content "hello hello")]))

(cl-assert
 (equal
  (let ((test-string "[ME]: [ME]: hello")) (org-ai--collect-chat-messages test-string))
  '[(:role user :content "hello")]))

;; (comment
;;   (with-current-buffer "org-ai-mode-test.org"
;;    (org-ai--collect-chat-messages (org-ai-get-block-content))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai)
