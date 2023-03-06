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

(defun org-ai-complete-block ()
  "Main command, normally bound to C-c C-c. When you are inside an
#+begin_ai...#+end_ai block, it will send the text content to the
OpenAI API and replace the block with the result."
  (interactive)
  (let* ((context (org-ai-special-block))
         (prompt (org-ai-get-block-content context)))
    (org-ai-stream-completion prompt :context context)))

(defvar org-ai--current-request-buffer nil
  "Internal var that stores the current request buffer.")

(defvar org-ai--current-request-callback nil
  "Internal var that stores the current request callback.")

(defvar org-ai--current-insert-position nil
  "Where to insert the result.")
(make-variable-buffer-local 'org-ai--current-insert-position)

(defvar org-ai--url-buffer-last-position nil
  "Local buffer var to store last read position")
(make-variable-buffer-local 'org-ai--url-buffer-last-position)
;; '(makunbound 'org-ai--url-buffer-last-position)

(cl-defun org-ai-stream-completion (prompt &optional &key model max-tokens temperature top-p frequency-penalty presence-penalty context)
  "Starts an server-sent event stream."
  (lexical-let* ((context (or context (org-ai-special-block)))
                 (buffer (current-buffer)))
    (let* ((content (org-ai-get-block-content context))
           (info (org-ai-get-block-info context))
           (model (or model (alist-get :model info)))
           (max-tokens (or max-tokens (alist-get :max-tokens info))))
      (setq org-ai--current-insert-position nil)
      (org-ai-stream-request prompt
                             (lambda (result)
                               (if-let ((error (plist-get result 'error)))
                                   (if-let ((message (plist-get error 'message))) (error message) (error error))
                                 (if-let* ((choice (aref (plist-get result 'choices) 0))
                                           (text (plist-get choice 'text)))
                                     (let ((text (decode-coding-string text 'utf-8)))
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
         (prompt (encode-coding-string prompt 'utf-8))
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

    (setq org-ai--current-request-callback callback)

    (setq org-ai--current-request-buffer
          (url-retrieve
           "https://api.openai.com/v1/completions"
           (lambda (events)
             (org-ai-reset-stream-state))))

    ;; (pop-to-buffer org-ai--current-request-buffer)

    (unless (member 'org-ai--url-request-on-change-function after-change-functions)
      (with-current-buffer org-ai--current-request-buffer
        (add-hook 'after-change-functions 'org-ai--url-request-on-change-function nil t)))))

(defun org-ai--url-request-on-change-function (beg end len)
  "Three arguments are passed to each function: the positions of
the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range."
  (with-current-buffer org-ai--current-request-buffer
    (save-excursion
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
             (error nil))))))))

(defun org-ai-reset-stream-state ()
  ""
  (interactive)
  (when (and org-ai--current-request-buffer (buffer-live-p org-ai--current-request-buffer))
    (with-current-buffer org-ai--current-request-buffer
      (remove-hook 'after-change-functions 'org-ai--url-request-on-change-function t)
      (setq org-ai--url-buffer-last-position nil)))
  (setq org-ai--current-request-callback nil)
  (setq org-ai--url-buffer-last-position nil))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-mode)
