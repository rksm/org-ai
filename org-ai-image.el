(require 'url-http)

(defvar data nil
  "")

(defcustom org-ai-image-director (expand-file-name "org-ai-images/" org-directory)
  "Directory where images are stored."
  :group 'org-ai
  :type 'directory)

(defun org-ai--image-save-base64-payload (data file-name)
  ""
  (let* ((data (alist-get 'data data))
         (data (aref data 0))
         (base64-string (alist-get 'b64_json data)))
    (with-temp-file file-name
      (insert (base64-decode-string base64-string)))))

(defun org-ai--image-save (data size prompt)
  ""
  (make-directory org-ai-image-director t)
  (let ((file-name (org-ai--make-up-new-image-file-name org-ai-image-director size)))
    (with-temp-file (string-replace ".png" ".txt" file-name) (insert prompt))
    (org-ai--image-save-base64-payload data file-name)
    file-name))

(defun org-ai--make-up-new-image-file-name (dir size &optional n)
  ""
  (let ((file-name (format "%s_%s_image%s.png"
                           (format-time-string "%Y%m%d" (current-time))
                           size
                           (if n (format "_%s" n) ""))))
    (if (file-exists-p (expand-file-name file-name dir))
        (org-ai--make-up-new-image-file-name dir size (1+ (or n 0)))
      (expand-file-name file-name dir))))

(defun org-ai--image-generate (prompt &optional size callback)
  (let* ((token org-ai-openai-api-token)
         (url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,token) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (n 1)
         (size (or size "256x256"))
         ;; (response-format "url")
         (response-format "b64_json")
         (url-request-data (json-encode (map-filter (lambda (x _) x)
                                                    `((prompt . ,prompt)
                                                      (n . ,n)
                                                      (response_format . ,response-format)
                                                      (size . ,size))))))
    (lexical-let ((size size)
                  (prompt prompt)
                  (callback callback))
      (url-retrieve
       "https://api.openai.com/v1/images/generations"
       (lambda (events)
         (goto-char url-http-end-of-headers)
         ;;(prog1 (setq data (json-read)) (kill-buffer))
         (let ((file (org-ai--image-save (json-read) size prompt)))
           (when callback
             (funcall callback file))
           ))))))

(defun org-ai-create-and-embed-image (context)
  ""
  (let* ((prompt (org-ai-get-block-content context))
         (prompt (encode-coding-string prompt 'utf-8))
         (info (org-ai-get-block-info context))
         (size (or (alist-get :size info) "256x256")))
    (lexical-let ((context context)
                  (buffer (current-buffer)))
      (org-ai--image-generate prompt size
                              (lambda (file)
                                (message "save %s" file)
                                (with-current-buffer buffer
                                  (save-excursion
                                    (let ((name (plist-get (cadr (org-ai-special-block)) :name))
                                          (contents-end (plist-get (cadr (org-ai-special-block)) :contents-end)))
                                      (goto-char contents-end)
                                      (next-line)
                                      (insert (format "[[file:%s]]\n" file))
                                      (org-display-inline-images)))))))))

(provide 'org-ai-image)
