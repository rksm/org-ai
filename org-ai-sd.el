;;; org-ai-sd.el --- Stable Diffusion web-UI / image generation  -*- lexical-binding: t; -*-

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

;; Stable Diffusion web UI  / image generation
;;
;; - https://github.com/AUTOMATIC1111/stable-diffusion-webui

;;; Code:

(defgroup org-ai-sd nil
  "Stable Diffusion web UI / image generation."
  :group 'org-ai)

(defcustom org-ai-sd-directory (expand-file-name "org-ai-images/" org-directory)
  "Directory where images are stored."
  :group 'org-ai-sd
  :type 'directory)

(defcustom org-ai-sd-endpoint-base nil
  "Base URL for the Stable Diffusion web UI API. Specify a URL like
\"http://your.server.name:7861/sdapi/v1/\". A trailing slash is required."
  :group 'org-ai-sd
  :type 'string)

(defcustom org-ai-sd-model-id nil
  "Stable diffusion checkpoints.
If not specified explicitly, the current setting will be used. A
list of available models can be found at (org-ai-sd-models)."
  :group 'org-ai-sd
  :type 'string)

(defvar org-ai-sd-txt2img-options
      `((negative_prompt . "worst quality, low qualitytext, multipul angle, longbody, lowres, bad anatomy, bad hands, missing fingers, cropped")
        (seed . -1)
        (subseed . -1)
        (subseed_strength . 0)
        (batch_size . 1)
        (steps . 20)
        (cfg_scale . 7)
        (sampler_name . "Euler a") ;; (org-ai-sd-samplers)
        (restore_faces . 0)
        ;; Hires.fix
        (enable_hr . 0)
        (denoising_strength . 0.7) ;; In img2img, refer even if it is not hr
        (hr_scale . 2)
        (hr_upscaler . "Latent") ;; (org-ai-sd-upscalers)
        (hr_second_pass_steps . 17))
      "Specify options for txt2img and img2txt.")
      ;; https://github.com/AUTOMATIC1111/stable-diffusion-webui/blob/master/modules/api/api.py
      ;; https://github.com/AUTOMATIC1111/stable-diffusion-webui/blob/master/modules/txt2img.py
      ;; https://github.com/AUTOMATIC1111/stable-diffusion-webui/blob/master/modules/img2img.py

(defun org-ai-sd-getter (name)
  "Function to check available options"
  (let* ((url-request-extra-headers `(("Content-Type" . "application/json")))
         (url-request-method "GET")
         (endpoint (concat org-ai-sd-endpoint-base name))
         (url-request-data (json-encode (map-filter (lambda (x _) x)
                                                    nil))))
    (url-retrieve
     endpoint
     (lambda (_events)
       (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
         (goto-char url-http-end-of-headers)
         (let ((json (json-read))
               (output-buffer (generate-new-buffer (concat "*" name "*"))))
           (with-current-buffer output-buffer
             (insert (pp-to-string json))
             (pop-to-buffer output-buffer))))))))

(defun org-ai-sd-models ()
  (interactive)
  (org-ai-sd-getter "sd-models"))

(defun org-ai-sd-cmd-flags ()
  (interactive)
  (org-ai-sd-getter "cmd-flags"))

(defun org-ai-sd-samplers ()
  (interactive)
  (org-ai-sd-getter "samplers"))

(defun org-ai-sd-upscalers ()
  (interactive)
  (org-ai-sd-getter "upscalers"))

(defun org-ai-sd-options ()
  (interactive)
  (org-ai-sd-getter "options"))

(defun org-ai--sd-save-base64-payload (base64-string file-name)
  "Write the base64 encoded payload `BASE64-STRING' to `FILE-NAME'."
  (with-temp-file file-name
    (insert (base64-decode-string base64-string))))

(defun org-ai--sd-images-save (data size &optional prompt)
  "Save the image `DATA' to into a file. Use `SIZE' to determine the file name.
Also save the `PROMPT' to a file."
  (make-directory org-ai-sd-directory t)
  (cl-loop for ea across (alist-get 'images data)
           collect (let ((file-name (org-ai--sd-make-up-new-image-file-name org-ai-sd-directory size)))
                     (when prompt (with-temp-file (string-replace ".png" ".txt" file-name) (insert prompt)))
                     (org-ai--sd-save-base64-payload ea file-name)
                     file-name)))

(defun org-ai--sd-make-up-new-image-file-name (dir size &optional n)
  "Make up a new file name for an image. Use `DIR' as the directory.
Use `SIZE' to determine the file name. If `N' is given, append it
to the file name."
  (let ((file-name (format "%s_%s_image%s.png"
                           (format-time-string "%Y%m%d" (current-time))
                           size
                           (if n (format "_%s" n) ""))))
    (if (file-exists-p (expand-file-name file-name dir))
        (org-ai--sd-make-up-new-image-file-name dir size (1+ (or n 0)))
      (expand-file-name file-name dir))))

(defun org-ai--sd-options-request (model-id)
  (let* ((url-request-extra-headers `(("Content-Type" . "application/json")))
         (url-request-method "POST")
         (endpoint (concat org-ai-sd-endpoint-base "options"))
         (url-request-data (json-encode (map-filter (lambda (x _) x)
                                                    `((sd_model_checkpoint . ,model-id)
                                                      ;;(sd_vae . "vae-ft-mse-840000-ema-pruned.ckpt")
                                                      )))))
    (url-retrieve
     endpoint
     (lambda (_events) nil))))

(cl-defun org-ai--sd-request (prompt &optional &key n size image-path callback)
  "Generate an image with `PROMPT'. Use `SIZE' to determine the size of the image.
`N' specifies the number of images to generate. If `CALLBACK' is
given, call it with the file name of the image as argument."
  (let* ((url-request-extra-headers `(("Content-Type" . "application/json")))
         (url-request-method "POST")
         (endpoint (concat org-ai-sd-endpoint-base (if image-path
                                                       "img2img"
                                                     "txt2img")))
         (image (if image-path
                    (concat "data:image/png;base64,"
                            (base64-encode-string
                             (with-temp-buffer
                               (insert-file-contents-literally image-path)
                               (buffer-substring-no-properties (point-min) (point-max)))))))
         (n (or n 1))
         (size (or size "512x512"))
         (k (string-match "x" size))
         (width (substring size 0 k))
         (height (substring size (+ k 1) (length size)))
         (url-request-data (json-encode (map-filter (lambda (x _) x)
                                                    `(,@(if image
                                                            `((init_images . (,image))))
                                                      (prompt . ,prompt)
                                                      (n_iter . ,n)
                                                      (width . ,width)
                                                      (height . ,height)
                                                      ,@org-ai-sd-txt2img-options)))))
    (url-retrieve
     endpoint
     (lambda (_events)
       (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
         (goto-char url-http-end-of-headers)
         (let ((files (org-ai--sd-images-save (json-read) size prompt)))
           (when callback
             (cl-loop for file in files
                      for i from 0
                      do (funcall callback file i)))))))))

(defun org-ai--sd-interrogate-request (image-path model callback)
  (let* ((url-request-extra-headers `(("Content-Type" . "application/json")))
         (url-request-method "POST")
         (endpoint (concat org-ai-sd-endpoint-base "interrogate"))
         (image (if image-path
                    (concat "data:image/png;base64,"
                            (base64-encode-string
                             (with-temp-buffer
                               (insert-file-contents-literally image-path)
                               (buffer-substring-no-properties (point-min) (point-max)))))))
         (url-request-data (json-encode (map-filter (lambda (x _) x)
                                                    `((image . ,image)
                                                      (model . ,model))))))
    (url-retrieve
     endpoint
     (lambda (_events)
       (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
         (goto-char url-http-end-of-headers)
         (funcall callback (cdr (assq 'caption (json-read)))))))))

(defun org-ai-sd--get-previous-image-path (number)
  "Return the path of the Nth previous embeded image before the cursor position."
  (interactive "nNumber of images to go back: ")
  (if (>= number 0)
      (let ((debug-on-error t))
        (error "Number must be negative."))
    (let ((result nil)
          (pos (point))
          (limit (point-min)))
      (save-excursion
        (dotimes (_ (abs number))
          (setq result nil)
          (while (and (not result) (re-search-backward "\\[\\[file:\\([^]]+\\)\\]" limit t))
            (let ((path (match-string 1)))
              (when (and (not (member path '("." ".."))))
                (setq result (substring-no-properties path)))))
          (setq limit (point-min)))
        (goto-char pos))
      (if (null result)
          (let ((debug-on-error t))
            (error "image not found"))
        result))))

(defun org-ai-sd--get-org-image-path (label)
  "Get the path of the image with the specified LABEL in the current org buffer."
  (catch 'found
    (org-element-map (org-element-parse-buffer) 'paragraph
      (lambda (paragraph)
        (when (string= (org-element-property :name paragraph) label)
          (org-element-map paragraph 'link
            (lambda (link)
              (when (string= (org-element-property :type link) "file")
                (throw 'found (org-element-property :path link))))))))
    (let ((debug-on-error t))
      (error "image not found"))))

(defun org-ai-create-and-embed-sd (context)
  "Create an image using Stable Diffusion web UI API with the prompt from the current block and embed the image in the current buffer. `CONTEXT` is the context object.

The function behaves as `img2img` when one of the named arguments `:image-path`, `:image-offset`, or `:image-ref` is specified, and as `txt2img` otherwise.

When `:image-path` is specified, the function processes the image specified by the path/name.

When `:image-offset` is specified with a negative integer value, the function references the nth previously embedded image.

When `:image-ref` is specified with a string value that corresponds to the reference name in org-mode for the image to be referenced, the function references the org-mode image with the given name.

If none of these arguments are specified, the function processes the prompt as `txt2img`."
  (let* ((prompt (encode-coding-string (org-ai-get-block-content context) 'utf-8))
         (info (org-ai-get-block-info context))
         (size (or (alist-get :size info) "512x512"))
         (n (or (alist-get :n info) 1))
         (image-path (alist-get :image-path info))
         (image-offset (alist-get :image-offset info))
         (image-ref (alist-get :image-ref info))
         (buffer (current-buffer)))
    (cond (image-path
           nil)
          (image-offset
           (setq image-path (org-ai-sd--get-previous-image-path image-offset)))
          (image-ref
           (setq image-path (org-ai-sd--get-org-image-path image-ref))))
    (if (and image-path (not (file-exists-p image-path)))
      (let ((debug-on-error t))
        (error "image not found")))
    (if org-ai-sd-model-id
        (org-ai--sd-options-request org-ai-sd-model-id))
    (if (null org-ai-sd-endpoint-base)
      (let ((debug-on-error t))
        (error "org-ai-sd-endpoint-base is not specified")))
    (org-ai--sd-request prompt
                       :n n
                       :size size
                       :image-path image-path
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

(defun org-ai--sd-clip-base (model callback)
  (let* ((image-path (org-ai-sd--get-previous-image-path -1)))
    (if (null org-ai-sd-endpoint-base)
      (let ((debug-on-error t))
        (error "org-ai-sd-endpoint-base is not specified")))
    (org-ai--sd-interrogate-request image-path
                                    model
                                    callback)))

(defun org-ai-sd-clip ()
  "Guess the prompt of the image just before org-mode by CLIP interrogator and save it to the kill ring."
  (interactive)
  (org-ai--sd-clip-base "clip"
                       (lambda (message)
                         (with-temp-buffer
                           (insert message)
                           (kill-ring-save (point-min) (point-max))
                           (message message)))))

(defun org-ai-sd-deepdanbooru ()
  "Guess the prompt of the image just before org-mode by DeepDanbooru interrogator and save it to the kill ring."
  (interactive)
  (org-ai--sd-clip-base "deepdanbooru"
                       (lambda (message)
                         (with-temp-buffer
                           (insert message)
                           (kill-ring-save (point-min) (point-max))
                           (message message)))))

(provide 'org-ai-sd)


;;; org-ai-sd.el ends here
