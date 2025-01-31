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

(defcustom org-ai-save-image-as-attachment nil
  "If set, images will be stored as attachments to current heading."
  :group 'org-ai
  :type 'bool)

(defcustom org-ai-image-directory (expand-file-name "org-ai-images/" org-directory)
  "Directory where images are stored."
  :group 'org-ai
  :type 'directory)

(defcustom org-ai-image-model "dall-e-3"
  "Model to use for image generation."
  :group 'org-ai
  :type '(choice (const :tag "DALL·E-3" "dall-e-3")
                 (const :tag "DALL·E-2" "dall-e-2")
                 (const :tag "default" nil)))

(defcustom org-ai-image-default-size "1024x1024"
  "Default size for generated images. Note that DALL·E-3 and
DALL·E-2 different and distinct image sizes. See
https://cookbook.openai.com/articles/what_is_new_with_dalle_3 for
more information."
  :group 'org-ai
  :type '(choice (const :tag "256x256" "256x256")
                 (const :tag "512x512" "512x512")
                 (const :tag "1024x1024" "1024x1024")
                 (const :tag "1792x1024" "1792x1024")
                 (const :tag "1024x1792" "1024x1792")))

(defcustom org-ai-image-default-count 1
  "How many images to generate by default."
  :group 'org-ai
  :type 'integer)

(defcustom org-ai-image-default-style 'natural
  "The default style for generated images. See
https://cookbook.openai.com/articles/what_is_new_with_dalle_3#new-styles
for more information."
  :group 'org-ai
  :type '(choice (const :tag "natural" natural)
                 (const :tag "vivid" vivid)))

(defcustom org-ai-image-default-quality 'standard
  "The default style for generated images. See
https://cookbook.openai.com/articles/what_is_new_with_dalle_3#standard-vs-hd-quality
for more information."
  :group 'org-ai
  :type '(choice (const :tag "standard" standard)
                 (const :tag "hd" hd)))

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
  (let ((timestamp (current-time)))
    (cl-loop for ea across (alist-get 'data data)
             collect (let* ((file-name (org-ai--make-up-new-image-file-name org-ai-image-directory size timestamp)))
                       (when prompt (with-temp-file (string-replace ".png" ".txt" file-name) (insert prompt)))
                       (org-ai--image-save-base64-payload (alist-get 'b64_json ea) file-name)
                       file-name))))

(defun org-ai--make-up-new-image-file-name (dir size timestamp &optional n)
  "Make up a new file name for an image. Use `DIR' as the directory.
Use `SIZE' and `TIMESTAMP' to determine the file name. If `N' is given, append it
to the file name."
  (let ((file-name (format "%s_%s_image%s.png"
                           (format-time-string "%Y%m%d%H%M%S" timestamp)
                           size
                           (if n (format "_%s" n) ""))))
    (if (file-exists-p (expand-file-name file-name dir))
        (org-ai--make-up-new-image-file-name dir size timestamp (1+ (or n 0)))
      (expand-file-name file-name dir))))

(defun org-ai--validate-image-size (model size)
  "Validate `SIZE' for `MODEL'."
  (let ((is-valid (cond
                   ((string-equal model "dall-e-3") (member size '("1024x1024" "1792x1024" "1024x1792")))
                   (t (member size '("256x256" "512x512" "1024x1024"))))))
    (unless is-valid
      (warn "Invalid image size %s for model %s" size model))))

(defun org-ai--validate-image-style (style)
  "Validate `STYLE'."
  (unless (member style '(natural vivid))
    (warn "Invalid image style %s" style)))

(defun org-ai--validate-image-quality (quality)
  "Validate `QUALITY'."
  (unless (member quality '(standard hd))
    (warn "Invalid image quality %s" quality)))

(defun org-ai--validate-image-model (model)
  "Validate `MODEL'."
  (unless (member model '("dall-e-3" "dall-e-2" nil))
    (warn "Invalid image model %s" model)))


(defvar org-ai--load-image-animation-stage 0)
(defvar org-ai--load-image-timer nil)

(defun org-ai--load-image-animation-update ()
  (let ((animation-frames '("." ".." "...")))
    (setq org-ai--load-image-animation-stage (mod (1+ org-ai--load-image-animation-stage)
                                                  (length animation-frames)))
    (message "org-ai generating image%s (press ctrl-g to cancel)" (nth org-ai--load-image-animation-stage animation-frames))))

(defun org-ai--load-image-start-animation ()
  (setq org-ai--load-image-timer (run-with-timer 1.0 1.0 #'org-ai--load-image-animation-update)))

(defun org-ai--load-image-stop-animation ()
  (when org-ai--load-image-timer
    (cancel-timer org-ai--load-image-timer)
    (setq org-ai--load-image-timer nil))
  (setq org-ai--load-image-animation-stage 0))

(defvar org-ai--current-request-buffer-for-image nil
  "Internal var that stores the current request buffer.
For image generation.")

(cl-defun org-ai--image-request (prompt &optional &key n size style quality model callback)
  "Generate an image with `PROMPT'. Use `SIZE' to determine the size of the image.
`N' specifies the number of images to generate. If `CALLBACK' is
given, call it with the file name of the image as argument."
  (org-ai--validate-image-model model)
  (org-ai--validate-image-size model size)
  (org-ai--validate-image-style style)
  (org-ai--validate-image-quality quality)
  (let* ((url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,(org-ai--openai-get-token)) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (endpoint org-ai-openai-image-generation-endpoint)
         (n (or n 1))
         (size (or size "256x256"))
         (response-format "b64_json")
         (url-request-data (json-encode (map-filter (lambda (x _) x)
                                                    `(,@(when model `((model . ,model)))
                                                      ,@(when style `((style . ,style)))
                                                      ,@(when quality `((quality . ,quality)))
                                                      (prompt . ,prompt)
                                                      (n . ,n)
                                                      (response_format . ,response-format)
                                                      (size . ,size))))))

    (org-ai-image-interrupt-current-request)

    (org-ai--load-image-start-animation)

    (setq org-ai--current-request-buffer-for-image
          (url-retrieve
           endpoint
           (lambda (_events)
             (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
               (goto-char url-http-end-of-headers)
               (setq org-ai--current-request-buffer-for-image nil)
               (org-ai--load-image-stop-animation)
               (let ((files (org-ai--images-save (json-read) size prompt)))
                 (when callback
                   (cl-loop for file in files
                            for i from 0
                            do (funcall callback file i))))))))))


(defun org-ai-image-interrupt-current-request ()
  "Interrupt the current request."
  (interactive)
  (when (and org-ai--current-request-buffer-for-image (buffer-live-p org-ai--current-request-buffer-for-image))
    (let (kill-buffer-query-functions)
      (kill-buffer org-ai--current-request-buffer-for-image))
    (setq org-ai--current-request-buffer-for-image nil))
  (org-ai--load-image-stop-animation))

(defun org-ai-create-and-embed-image (context)
  "Create an image with the prompt from the current block.
Embed the image in the current buffer. `CONTEXT' is the context
object."
  (let* ((prompt (encode-coding-string (org-ai-get-block-content context) 'utf-8))
         (info (org-ai-get-block-info context))
         (size (or (alist-get :size info)
                   (org-entry-get-with-inheritance "IMAGE_SIZE")
                   org-ai-image-default-size))
         (model (or (alist-get :model info)
                    (org-entry-get-with-inheritance "IMAGE_MODEL")
                    org-ai-image-model))
         (n (or (alist-get :n info)
                (when-let ((it (org-entry-get-with-inheritance "IMAGE_COUNT"))) (string-to-number it))
                org-ai-image-default-count))
         (style (or (when-let ((it (alist-get :style info))) (intern it))
                    (when-let ((it (org-entry-get-with-inheritance "IMAGE_STYLE"))) (intern it))
                    org-ai-image-default-style))
         (quality (or (when-let ((it (alist-get :quality info))) (intern it))
                      (when-let ((it (org-entry-get-with-inheritance "IMAGE_QUALITY"))) (intern it))
                      org-ai-image-default-quality))
         (buffer (current-buffer)))
    (org-ai--image-request prompt
                           :model model
                           :n n
                           :size size
                           :style style
                           :quality quality
                           :callback (lambda (file i)
                                       (message "saved %s" file)
                                       (with-current-buffer buffer
                                         (save-excursion
                                           (let ((name (plist-get (cadr (org-ai-special-block)) :name))
                                                 (contents-end (if (fboundp 'org-element-contents-end)
                                                                   (org-element-contents-end (org-ai-special-block))
                                                                 (plist-get (cadr (org-ai-special-block)) :contents-end))))
                                             (goto-char contents-end)
                                             (forward-line)
                                             (when name
                                               (insert (format "#+NAME: %s%s\n" name (if (> n 1) (format "_%s" i) "") )))
                                             (insert (org-link-make-string (format "%s:%s"
                                                                                   (if org-ai-save-image-as-attachment "attachment" "file")
                                                                                   (if org-ai-save-image-as-attachment (file-name-nondirectory file) file))))
                                             (insert "\n")
                                             (when org-ai-save-image-as-attachment
                                               (org-attach-attach file nil 'mv)
                                               (when prompt (org-attach-attach (string-replace ".png" ".txt" file) nil 'mv))
                                               )
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
                                                     (insert (org-link-make-string (format "%s:%s"
                                                                                           (if org-ai-save-image-as-attachment "attachment" "file")
                                                                                           (if org-ai-save-image-as-attachment (file-name-nondirectory file) file))))
                                                     (insert "\n")
                                                     (when org-ai-save-image-as-attachment
                                                       (org-attach-attach file nil 'mv))
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
