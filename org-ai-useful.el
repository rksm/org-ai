;;; org-ai-useful.el --- A few useful functions and commands -*- lexical-binding: t; -*-

(defun org-ai-prompt (prompt &optional sys-prompt output-buffer)
  "Prompt for an input, send it to chatgpt and insert the response
in the current buffer and select it."
  (interactive
   (list
    (read-string "What's up? ")))
  (lexical-let ((output-buffer (or output-buffer (current-buffer)))
                (start-pos (point)))
    (let* ((sys-input (if sys-prompt (format "[SYS]: %s\n" sys-prompt)))
           (input (format "%s\n[ME]: %s" sys-input prompt)))
      (org-ai-stream-request :messages (org-ai--collect-chat-messages input)
                             :model org-ai-default-chat-model
                             :callback (lambda (response)
                                         (when-let* ((choices (or (alist-get 'choices response)
                                                                  (plist-get response 'choices)))
                                                     (choice (aref choices 0)))
                                           (let ((delta (plist-get choice 'delta)))
                                             (cond
                                              ((plist-get delta 'content)
                                               (let ((text (plist-get delta 'content)))
                                                 (with-current-buffer output-buffer
                                                   (insert (decode-coding-string text 'utf-8)))))
                                              ((plist-get choice 'finish_reason)
                                               (with-current-buffer output-buffer
                                                 (set-mark (point))
                                                 (goto-char start-pos)))))))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-useful)

;;; org-ai-useful.el ends here
