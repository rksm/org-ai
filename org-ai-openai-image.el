;;; org-ai-openai-image.el --- DALL-E / image generation  -*- lexical-binding: t; -*-

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

;; DALL-E / image generation

;;; Code:

(require 'org-ai-openai)

(defcustom org-ai-image-directory (expand-file-name "org-ai-images/" org-directory)
  "Directory where images are stored."
  :group 'org-ai
  :type 'directory)

(defvar org-ai-openai-image-generation-endpoint "https://api.openai.com/v1/images/generations")

(defvar org-ai-openai-image-variation-endpoint "https://api.openai.com/v1/images/variations")

(defun org-ai--image-save-base64-payload (base64-string file-name)
  "Write the base64 encoded payload `BASE64-STRING' to `FILE-NAME'."
  (with-temp-file file-name
    (insert (base64-decode-string base64-string))))

(defun org-ai--images-save (data size &optional prompt)
  "Save the image `DATA' to into a file. Use `SIZE' to determine the file name.
Also save the `PROMPT' to a file."
  (make-directory org-ai-image-directory t)
  (cl-loop for ea across (alist-get 'data data)
           collect (let ((file-name (org-ai--make-up-new-image-file-name org-ai-image-directory size)))
                     (when prompt (with-temp-file (string-replace ".png" ".txt" file-name) (insert prompt)))
                     (org-ai--image-save-base64-payload (alist-get 'b64_json ea) file-name)
                     file-name)))

(defun org-ai--make-up-new-image-file-name (dir size &optional n)
  "Make up a new file name for an image. Use `DIR' as the directory.
Use `SIZE' to determine the file name. If `N' is given, append it
to the file name."
  (let ((file-name (format "%s_%s_image%s.png"
                           (format-time-string "%Y%m%d" (current-time))
                           size
                           (if n (format "_%s" n) ""))))
    (if (file-exists-p (expand-file-name file-name dir))
        (org-ai--make-up-new-image-file-name dir size (1+ (or n 0)))
      (expand-file-name file-name dir))))

(cl-defun org-ai--image-request (prompt &optional &key n size callback)
  "Generate an image with `PROMPT'. Use `SIZE' to determine the size of the image.
`N' specifies the number of images to generate. If `CALLBACK' is
given, call it with the file name of the image as argument."
  (let* ((url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,(org-ai--openai-get-token)) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (endpoint org-ai-openai-image-generation-endpoint)
         (n (or n 1))
         (size (or size "256x256"))
         (response-format "b64_json")
         (url-request-data (json-encode (map-filter (lambda (x _) x)
                                                    `((prompt . ,prompt)
                                                      (n . ,n)
                                                      (response_format . ,response-format)
                                                      (size . ,size))))))
    (url-retrieve
     endpoint
     (lambda (_events)
       (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
         (goto-char url-http-end-of-headers)
         (let ((files (org-ai--images-save (json-read) size prompt)))
           (when callback
             (cl-loop for file in files
                      for i from 0
                      do (funcall callback file i)))))))))

(defun org-ai-create-and-embed-image (context)
  "Create an image with the prompt from the current block.
Embed the image in the current buffer. `CONTEXT' is the context
object."
  (let* ((prompt (encode-coding-string (org-ai-get-block-content context) 'utf-8))
         (info (org-ai-get-block-info context))
         (size (or (alist-get :size info) "256x256"))
         (n (or (alist-get :n info) 1))
         (buffer (current-buffer)))
    (org-ai--image-request prompt
                           :n n
                           :size size
                           :callback (lambda (file i)
                                       (message "saved %s" file)
                                       (with-current-buffer buffer
                                         (save-excursion
                                           (let ((name (plist-get (cadr (org-ai-special-block)) :name))
                                                 (contents-end (plist-get (cadr (org-ai-special-block)) :contents-end)))
                                             (goto-char contents-end)
                                             (forward-line)
                                             (when name
                                               (insert (format "#+NAME: %s%s\n" name (if (> n 0) (format "_%s" i) "") )))
                                             (insert (format "[[file:%s]]\n" file))
                                             (org-display-inline-images))))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; image variation

(defun org-ai-image-variation (path n size)
  "Generate `N' variations of the image at point (a link pointing to a file).
Use `SIZE' to determine the size of the image. `PATH' is the path
to the image."
  (interactive (list (let ((at-point (org-ai--image-variation--get-path-of-link-at-point))) (read-file-name "image: " nil at-point nil at-point))
                     (read-number "n: " 1)
                     (completing-read "size: " '("256x256" "512x512" "1024x1024") nil t "256x256" nil "256x256")))
  (let ((buffer (current-buffer)))
    (org-ai--image-variation-request path
                                     :n n
                                     :size size
                                     :callback (lambda (file _i)
                                                 (message "saved %s" file)
                                                 (with-current-buffer buffer
                                                   (save-excursion
                                                     (move-end-of-line 1)
                                                     (insert (format "\n\n[[file:%s]]\n" file))
                                                     (org-display-inline-images)))))))

(cl-defun org-ai--image-variation-request (image-file-path &key n size callback)
  "Generate an image similar to `IMAGE-FILE-PATH'.
Use `SIZE' to determine the size of the image. `N' specifies the
number of images to generate. If `CALLBACK' is given, call it
with the file name of the image as argument. Note this requries
curl to be installed."
  (unless (executable-find "curl")
    (error "Unable to find curl"))
  (let ((command (format "curl --silent %s \\
                         -H 'Authorization: Bearer %s' \\
                         -F image='@%s' \\
                         -F n=%s \\
                         -F size=\"%s\" \\
                         -F response_format=\"b64_json\""
                         org-ai-openai-image-variation-endpoint
                         (org-ai--openai-get-token)
                         image-file-path
                         n
                         size)))

    (with-temp-buffer
      (condition-case err
          (progn (shell-command command (current-buffer))
                 (goto-char (point-min))
                 (let ((data (json-read)))
                   (if (alist-get 'error data)
                       (error (alist-get 'error data))
                       (let ((files (org-ai--images-save data size)))
                            (when callback
                              (cl-loop for file in files
                                       for i from 0
                                       do (funcall callback file i)))))))

        (error (let ((buffer-content (buffer-string))
                     (error-buffer (get-buffer-create "*org-ai-image-variation-error*")))
                 (with-current-buffer error-buffer
                   (erase-buffer)
                   (insert buffer-content))
                 (pop-to-buffer error-buffer))
               (error err))))))

(defun org-ai-open-account-usage-page ()
  "Open web browser with the OpenAI account usage page.
So you now how deep you're in the rabbit hole."
  (interactive)
  (browse-url "https://platform.openai.com/account/usage"))

(defun org-ai--image-variation--get-path-of-link-at-point ()
  "Read the path of the link at point.
Return nil if there is no link at point."
  (let* ((context
	  ;; Only consider supported types, even if they are not the
	  ;; closest one.
	  (org-element-lineage (org-element-context) '(link) t)))
    (when context
        (org-element-property :path context))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-openai-image)

;;; org-ai-openai-image.el ends here

