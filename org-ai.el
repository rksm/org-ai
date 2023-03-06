(defcustom org-ai-openai-api-token nil "Your OpenAI API token. You can retrieve it at https://platform.openai.com/account/api-keys"
  :type 'string
  :group 'org-ai)

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
  ""
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

(defun org-ai-complete-block ()
  ""
  (interactive)
  (let* ((context (org-ai-special-block))
         (prompt (org-ai-get-block-content context)))
    (org-ai-stream-completion prompt :context context)))

(defvar org-ai--current-request-buffer nil
  "")

(defvar org-ai--current-request-callback nil
  "")

(defvar org-ai--current-insert-position nil
  "")
(make-variable-buffer-local 'org-ai--current-insert-position)

(defvar org-ai--url-buffer-last-position nil
  "Local buffer var to store last read position")
(make-variable-buffer-local 'org-ai--url-buffer-last-position)
;; '(makunbound 'org-ai--url-buffer-last-position)

(cl-defun org-ai-stream-completion (prompt &optional &key model max-tokens temperature top-p frequency-penalty presence-penalty context)
  ""
  (lexical-let* ((context (or context (org-ai-special-block)))
                 (buffer (current-buffer)))
    (let* ((content (org-ai-get-block-content context))
           (info (org-ai-get-block-info context))
           (model (or model (alist-get :model info)))
           (max-tokens (or max-tokens (alist-get :max-tokens info))))
      ;; (message "%s model=%s" prompt model)
      (setq org-ai--current-insert-position nil)
      (org-ai-stream-request prompt
                             (lambda (result)
                               ;; (message "completion %s" result)
                               (if-let ((error (plist-get result 'error)))
                                   (if-let ((message (plist-get error 'message))) (error message) (error error))
                                 (if-let* ((choice (aref (plist-get result 'choices) 0))
                                           (text (plist-get choice 'text)))
                                     (let (;;(text (format "\n%s\n" (string-trim text)))
                                           (text text))
                                       (with-current-buffer buffer
                                         (let ((pos (or org-ai--current-insert-position (org-element-property :contents-end context))))
                                           (save-excursion
                                             (goto-char pos)
                                             (when (string-suffix-p "#+end_ai" (buffer-substring-no-properties (point) (line-end-position)))
                                               (insert "\n")
                                               (backward-char))
                                             (insert text)
                                             (setq org-ai--current-insert-position (point)))))))))
                             :model model
                             :max-tokens max-tokens
                             :temperature temperature
                             :top-p top-p
                             :frequency-penalty frequency-penalty
                             :presence-penalty presence-penalty))))

(cl-defun org-ai-stream-request (prompt callback &optional &key model max-tokens temperature top-p frequency-penalty presence-penalty)
  ""
  (let* ((token org-ai-openai-api-token)
         (url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,token) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (url-request-data
          (json-encode (map-filter (lambda (x _) x)
                                   `((prompt . ,prompt)
                                     (model . ,model)
                                     (stream . t)
                                     ,@(when max-tokens        `((max_tokens . ,max-tokens)))
                                     ,@(when temperature       `(temperature . ,temperature))
                                     ,@(when top-p             `(top_p . ,top-p))
                                     ,@(when frequency-penalty `(frequency_penalty . ,frequency-penalty))
                                     ,@(when presence-penalty  `(presence_penalty . ,presence-penalty)))))))
    ;; (message "POST %s" url-request-data)

    (setq org-ai--current-request-callback callback)

    (setq org-ai--current-request-buffer
          (url-retrieve
           "https://api.openai.com/v1/completions"
           (lambda (events)
             (goto-char url-http-end-of-headers)
             (setq last-result-raw (buffer-substring-no-properties (point) (point-max)))
             (let ((json-object-type 'plist)
                   (json-key-type 'symbol)
                   (json-array-type 'vector))
               (let ((result (json-read)))
                 ;; (message "RESULT %s"result)
                 (setq last-results (append last-results (list result)))
                 (setq last-result result)
                 ;; (funcall callback result)
                 (org-ai-reset-stream-state))))))

    ;; (pop-to-buffer org-ai--current-request-buffer)

    (unless (member 'org-ai--url-request-on-change-function after-change-functions)
      (with-current-buffer org-ai--current-request-buffer
        (add-hook 'after-change-functions 'org-ai--url-request-on-change-function nil t)))))

(defvar changes nil "")
(setq changes nil)

(defun org-ai--url-request-on-change-function (beg end len)
  "Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range."
  (with-current-buffer org-ai--current-request-buffer
    ;;(message "CHANGED <<<%s>>>" (buffer-substring-no-properties beg end))
    ;;(setq changes (append changes (list (buffer-substring-no-properties beg end))))

    (if org-ai--url-buffer-last-position
        (goto-char org-ai--url-buffer-last-position)
      (goto-char (point-min)))
    (search-forward "data: " nil t)
    (let* ((line (buffer-substring-no-properties (point) (line-end-position))))
      (if (string= line "[DONE]")
          (progn
            (org-ai-reset-stream-state)
            (message "org-ai request done"))
        (let ((json-object-type 'plist)
              (json-key-type 'symbol)
              (json-array-type 'vector))
          ;; try
          (condition-case err
              (let ((data (json-read)))
                (setq org-ai--url-buffer-last-position (point))
                (when org-ai--current-request-callback
                  (funcall org-ai--current-request-callback data)))
              (error nil))
          )))))

(defun org-ai-reset-stream-state ()
  ""
  (interactive)
  (when (and org-ai--current-request-buffer (buffer-live-p org-ai--current-request-buffer))
    (with-current-buffer org-ai--current-request-buffer
      (remove-hook 'after-change-functions 'org-ai--url-request-on-change-function t)
      (setq org-ai--url-buffer-last-position nil)))
  (setq org-ai--current-request-callback nil)
  (setq org-ai--url-buffer-last-position nil))

(comment
  (pop-to-buffer org-ai--current-request-buffer)
  (org-ai-stream-completion "foo" :model "bar" :max_tokens 100)

  (with-current-buffer "org-ai-mode-test.org"
    (list
     (org-ai-get-block-info)
     (org-ai-get-block-content))

    )


  





(nth 0 changes)
(nth 1 changes)
(alist-get :foo '((:foo . "bar2")))

(special-block
 (:type "ai" :begin 81 :end 115 :contents-begin 102 :contents-end 106 :post-blank 0 :post-affiliated 81 :parent nil))

(special-block
 (:type "ai" :begin 30 :end 82 :contents-begin 66 :contents-end 73 :post-blank 0 :post-affiliated 46 :name "testing" :parent nil))


  (require 'url)

;; (id
;;  cmpl-6qsO3eQ07TBL9seMPSRKS4zVb4qrN
;;  object text_completion
;;  created 1678060139
;;  model text-davinci-003
;;  choices [(text
;;            This is a test sentence used to demonstrate how a test sentence is
;;            index 0
;;            logprobs nil
;;            finish_reason length)]
;;  usage (prompt_tokens
;;         4 completion_tokens
;;         15
;;         total_tokens 19))


  (setq last-results nil)

  (let* ((token org-ai-openai-api-token)
         (url-request-extra-headers `(("Authorization" . ,(string-join `("Bearer" ,token) " "))
                                      ("Content-Type" . "application/json")))
         (url-request-method "POST")
         (url-request-data
          (json-encode `((prompt . "This is a test")
                         (model . "text-davinci-003")
                         (max_tokens . 100)
                         (temperature . 0.9)
                         (top_p . 1)
                         (frequency_penalty . 0)
                         (presence_penalty . 0)))))
    (url-retrieve
           "https://api.openai.com/v1/completions"
           (lambda (events)
             (goto-char url-http-end-of-headers)
             (let ((json-object-type 'plist)
                   (json-key-type 'symbol)
                   (json-array-type 'vector))
               (let ((result (json-read)))
                 ;; (message "RESULT %s"result)
                 (setq last-results (append last-results (list result)))
                 )))))


(id "cmpl-6qtTxXTtUtfR5IcgVowqitQn5rQhT" object "text_completion" created "1678064349" model "text-davinci-003" choices [(text The weather depends on where you are. It could be sunny, rainy, cold, or hot. index 0 logprobs nil finish_reason stop)] usage (prompt_tokens 5 completion_tokens 22 total_tokens 27))
(alist-get 'id (last last-results))
(plist-get last-result 'id)
last-result
last-result-raw
(if-let* ((choice (aref (plist-get last-result 'choices) 0))
          (text (plist-get choice 'text)))
    (if (string-suffix-p "\n" text) text (concat text "\n"))))

(provide 'org-ai-mode)
