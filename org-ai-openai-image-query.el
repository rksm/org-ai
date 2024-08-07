;;; org-ai-openai-image-query.el --- Send base64 or URL image and question to OpenAI -*- lexical-binding: t; -*-

;;; Code:

;; TODO use file path or url at point as default
(require 'org-ai-openai)

(defcustom org-ai-image-query-model "gpt-4o-mini"
  "The model to use for image queries."
  :group 'org-ai
  :type '(choice (const :tag "gpt-4o-mini" "gpt-4o-mini")
          (const :tag "gpt-4o" "gpt-4o")
          (const :tag "gpt-4-turbo" "gpt-4-turbo")))

(defvar org-ai-openai-image-query-endpoint "https://api.openai.com/v1/chat/completions"
  "Endpoint for querying images with OpenAI.")

(defun org-ai--send-image-query (image-content question callback)
  "Send IMAGE-CONTENT and QUESTION to the OpenAI API.
Calls CALLBACK with the response."
  (let ((url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,(org-ai--openai-get-token)) " "))
                                     ("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data (json-encode `(("model" . ,org-ai-image-query-model)
                                         ("messages" . [(("role" . "user")
                                                         ("content" . [((type . "text") (text . ,question))
                                                                       ,image-content]))])
                                         ("max_tokens" . ,(or org-ai-default-max-tokens 300))))))
    (url-retrieve
     org-ai-openai-image-query-endpoint
     (lambda (_status)
       (goto-char url-http-end-of-headers)
       (let ((response (json-read)))
         (if response
             (funcall callback response)
           (message "Failed to get a valid response from OpenAI API.")))))))

(defun org-ai--send-base64-image-query (base64-image question callback)
  "Send a BASE64-IMAGE and QUESTION to the OpenAI API.
Calls CALLBACK with the response."
  (org-ai--send-image-query
   `((type . "image_url")
     (image_url . (("url" . ,(concat "data:image/jpeg;base64," base64-image)))))
   question callback))

(defun org-ai--send-url-image-query (image-url question callback)
  "Send an IMAGE-URL and QUESTION to the OpenAI API.
Calls CALLBACK with the response."
  (org-ai--send-image-query
   `((type . "image_url")
     (image_url . (("url" . ,image-url))))
   question callback))

(defun org-ai--handle-openai-response (response)
  "Handle the RESPONSE from OpenAI API."
  (let ((choices (cdr (assq 'choices response))))
    (if choices
        ;; TODO put into current buffer, not message
        (message "OpenAI API response: %S" choices)
      (message "OpenAI API returned an unexpected response: %S" response))))

(defun org-ai--get-image-path-or-url ()
  "Prompt the user for a non-empty image path or URL."
  ;; TODO make file path or url nicer to use
  (let ((image-path-or-url ""))
    (while (or (string-empty-p image-path-or-url)
               (and (not (string-match-p "^https?://" image-path-or-url))
                    (not (file-exists-p image-path-or-url))))
      (setq image-path-or-url (read-string "Image file path or URL (must be non-empty): ")))
    image-path-or-url))

(defun org-ai--get-question ()
  "Prompt the user for a non-empty question."
  (let ((question ""))
    (while (string-empty-p question)
      (setq question (read-string "Question (must be non-empty): ")))
    question))

;;;###autoload
(defun org-ai-query-image ()
  "Query OpenAI API with a BASE64 encoded image path or URL and a QUESTION."
  (interactive)
  (let ((image-path-or-url (org-ai--get-image-path-or-url))
        (question (org-ai--get-question)))
    (if (string-match-p "^https?://" image-path-or-url)
        (org-ai--send-url-image-query image-path-or-url question #'org-ai--handle-openai-response)
      (let ((base64-image (with-temp-buffer
                            (insert-file-contents-literally image-path-or-url)
                            (base64-encode-region (point-min) (point-max))
                            (buffer-string))))
        (org-ai--send-base64-image-query base64-image question #'org-ai--handle-openai-response)))))

(provide 'org-ai-openai-image-query)

;;; org-ai-openai-image-query.el ends here
