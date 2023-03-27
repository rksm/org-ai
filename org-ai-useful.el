;;; org-ai-useful.el --- A few useful functions and commands -*- lexical-binding: t; -*-

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

;; None specific org commands.

;;; Code:

(defvar yas-snippet-dirs)

(defun org-ai-install-yasnippets ()
  "Installs org-ai snippets."
  (interactive)
  (let ((snippet-dir (expand-file-name "snippets/"
                                       (file-name-directory (locate-library "org-ai")))))
    (unless (boundp 'yas-snippet-dirs)
      (setq yas-snippet-dirs nil))
    (add-to-list 'yas-snippet-dirs snippet-dir t)
    (when (fboundp 'yas-load-directory)
      (yas-load-directory snippet-dir))))

(cl-defun org-ai-prompt (prompt &optional &key sys-prompt output-buffer select-output)
  "Prompt for a gpt input, insert the response in current buffer.
`PROMPT' is the prompt to use.
`SYS-PROMPT' is the system prompt to use.
`OUTPUT-BUFFER' is the buffer to insert the response in.
n`SELECT-OUTPUT' is whether to mark the output."
  (interactive
   (list (read-string "What's up? ")))
  (let ((output-buffer (or output-buffer (current-buffer)))
        (start-pos (point)))
    (let* ((sys-input (if sys-prompt (format "[SYS]: %s\n" sys-prompt)))
           (input (format "%s\n[ME]: %s" sys-input prompt)))
      (org-ai-stream-request :messages (org-ai--collect-chat-messages input)
                             :model org-ai-default-chat-model
                             :callback (lambda (response)
                                         (if-let* ((choices (or (alist-get 'choices response)
                                                                (plist-get response 'choices)))
                                                   (choice (aref choices 0)))
                                             (let ((delta (plist-get choice 'delta)))
                                               (cond
                                                ((plist-get delta 'role)
                                                 (let ((role (plist-get delta 'role)))
                                                   (run-hook-with-args 'org-ai-after-chat-insertion-hook 'role role)))
                                                ((plist-get delta 'content)
                                                 (let ((text (plist-get delta 'content)))
                                                   (with-current-buffer output-buffer
                                                     (insert (decode-coding-string text 'utf-8)))
                                                   (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text)))
                                                ((plist-get choice 'finish_reason)
                                                 (when select-output
                                                   (with-current-buffer output-buffer
                                                     (set-mark (point))
                                                     (goto-char start-pos))))))
                                           (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end "")))))))

(defun org-ai-summarize (start end)
  "Ask ChatGPT for a summary of the marked text.
`START' is the buffer position of the start of the text to summarize.
`END' is the buffer position of the end of the text to summarize."
  (interactive "r")
  (let* ((result-buffer (get-buffer-create (generate-new-buffer-name "*summary*")))
         (text (encode-coding-string (buffer-substring-no-properties start end) 'utf-8))
         (prompt (format "Summarize the following text:\n\n%s" text)))
    (pop-to-buffer result-buffer)
    (toggle-truncate-lines)
    (org-ai-prompt prompt :output-buffer result-buffer)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-useful)

;;; org-ai-useful.el ends here
