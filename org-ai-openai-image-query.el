;;; org-ai-openai-image-query.el --- Send base64 image and question to OpenAI -*- lexical-binding: t; -*-

;;; Code:

(require 'org-ai-openai)

(defcustom org-ai-image-query-model "gpt-4o-mini"
  "The model to use for image queries."
  :group 'org-ai
  :type '(choice (const :tag "GPT-4o-mini" "gpt-4o-mini")
          (const :tag "GPT-4o" "gpt-4o")
          (const :tag "GPT-4-turbo" "gpt-4-turbo")))

(defvar org-ai-openai-image-query-endpoint "https://api.openai.com/v1/chat/completions"
  "Endpoint for querying images with OpenAI.")

(defun org-ai--send-image-query (base64-image question callback)
  "Send BASE64-IMAGE and QUESTION to the OpenAI API.
Calls CALLBACK with the response."
  (let* ((url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,(org-ai--openai-get-token)) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (url-request-data (json-encode `(("model" . ,org-ai-image-query-model)
                                          ("messages" . [((role . "user")
                                                          (content . [
                                                                      ((type . "text")
                                                                       (text . ,question))
                                                                      ((type . "image_url")
                                                                       (image_url . ((url . ,(concat "data:image/jpeg;base64," base64-image)))))])
                                                          )])
                                          ("max_tokens" . 300)))))
    (url-retrieve
     org-ai-openai-image-query-endpoint
     (lambda (_status)
       (goto-char url-http-end-of-headers)
       (let ((response (json-read)))
         (if response
             (funcall callback response)
           (message "Failed to get a valid response from OpenAI API.")))))))

;;;###autoload
(defun org-ai-query-image (image-path question)
  "Query OpenAI API with a BASE64 encoded IMAGE-PATH and a QUESTION."
  (interactive "fImage file path: \nsQuestion: ")
  (let ((base64-image (with-temp-buffer
                        (insert-file-contents-literally image-path)
                        (base64-encode-region (point-min) (point-max))
                        (buffer-string))))
    (org-ai--send-image-query base64-image question
                              (lambda (response)
                                (let ((choices (cdr (assq 'choices response))))
                                  (if choices
                                      (message "OpenAI API response: %S" choices)
                                    (message "OpenAI API returned an unexpected response: %S" response)))))))

(provide 'org-ai-openai-image-query)

;;; org-ai-openai-image-query.el ends here
