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

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; snippet helpers
(defvar yas-snippet-dirs)

(defvar org-ai-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer-and-window)))
    map)
  "Keymap for `org-ai-output-mode'.")

(define-minor-mode org-ai-output-mode
  "Minor mode for buffers showing org-ai output."
  :init-value nil
  :keymap org-ai-output-mode-map
  :group 'org-ai
  (read-only-mode 1))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; just prompt

(defcustom org-ai-talk-spoken-input nil
  "Whether to use speech input.
Whether to use speech input for `org-ai-prompt' and
`org-ai-talk-on-region' commands. See `org-ai-talk' for the
details and implementation."
  :type 'boolean
  :group 'org-ai-talk)

(defcustom org-ai-talk-confirm-speech-input nil
  "Ask for confirmation before sending speech input to AI?"
  :type 'boolean
  :group 'org-ai-talk)

(defun org-ai-confirm-send (prompt input)
  "Show `PROMPT' and `INPUT' and ask for confirmation.
Will always return t if `org-ai-talk-confirm-speech-input' is nil."
  (if org-ai-talk-confirm-speech-input
      (let ((window-config (current-window-configuration))
            (buf (get-buffer-create "*org-ai-confirm*")))
        (unwind-protect
            (progn (pop-to-buffer buf)
                   (erase-buffer)
                   (insert prompt)
                   (insert "\n")
                   (insert input)
                   (prog1
                       (y-or-n-p (format "Send to AI?"))
                     (kill-buffer buf)))
          (set-window-configuration window-config)))
    t))

(defmacro org-ai-with-input-or-spoken-text (prompt input &rest body)
  "Macro to optionally use speech input.
`PROMPT' is the prompt to ask the user for.
`INPUT' is the variable to bind the input to.
`BODY' is the body to execute with `INPUT' bound."
  (declare (indent 2))
  `(if ,input
       (progn
         ,@body)
     (if (fboundp 'org-ai-talk--record-and-transcribe-speech)
         (org-ai-talk--record-and-transcribe-speech (lambda (,input)
                                                      (when (org-ai-confirm-send ,prompt ,input)
                                                        ,@body))
                                                    ,prompt)
       (error "Module not loaded: org-ai-talk"))))

(defvar-local org-ai-prompt--last-insertion-marker nil)

(defun org-ai-prompt--insert (output-buffer text &optional follow)
  "Insert `TEXT' in `OUTPUT-BUFFER'.
`FOLLOW' is whether to move point to the end of the inserted text."
  (cl-flet ((insert-fn (text)
                       (unless org-ai-prompt--last-insertion-marker
                         (setq org-ai-prompt--last-insertion-marker (point-marker)))
                       (goto-char org-ai-prompt--last-insertion-marker)
                       (let ((inhibit-read-only t))
                         (insert (decode-coding-string text 'utf-8)))
                       (set-marker org-ai-prompt--last-insertion-marker (point))))
    (with-current-buffer output-buffer
      (if follow
          (insert-fn text)
        (save-excursion
          (insert-fn text))))))

(cl-defun org-ai-prompt (prompt &optional &key sys-prompt output-buffer select-output follow callback)
  "Prompt for a gpt input, insert the response in current buffer.
`PROMPT' is the prompt to use.
`SYS-PROMPT' is the system prompt to use.
`OUTPUT-BUFFER' is the buffer to insert the response in.
`SELECT-OUTPUT' is whether to mark the output.
`CALLBACK' is a function to call after the response is inserted.
`FOLLOW' is whether to move point to the end of the inserted text."
  (interactive
   (list (unless org-ai-talk-spoken-input (read-string "What do you want to know? " nil 'org-ai-prompt-history))))

  (org-ai-with-input-or-spoken-text "What do you want to know?" prompt
    (let ((output-buffer (or output-buffer (current-buffer)))
          (start-pos-marker (point-marker)))
      (let* ((sys-input (if sys-prompt (format "[SYS]: %s\n" sys-prompt)))
             (input (format "%s\n[ME]: %s" sys-input prompt)))
        (with-current-buffer output-buffer
          (setq org-ai-prompt--last-insertion-marker (point-marker)))
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
                                                     (org-ai-prompt--insert output-buffer text follow)
                                                     (run-hook-with-args 'org-ai-after-chat-insertion-hook 'text text)))
                                                  ((plist-get choice 'finish_reason)
                                                   (when select-output
                                                     (with-current-buffer output-buffer
                                                       (set-mark (point))
                                                       (goto-char start-pos-marker))))))
                                             (setq org-ai-prompt--last-insertion-marker nil)
                                             (run-hook-with-args 'org-ai-after-chat-insertion-hook 'end "")
                                             (when callback (with-current-buffer output-buffer (funcall callback))))))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; org-ai-on-region

(defcustom org-ai-on-region-file nil
  "Optional file used to store the `org-ai-on-region' conversations in.
If nil, a buffer with no file backing is used. If a file is
specified, new conversations are appended to the file or
function's output. Function should take no arguments and return a
filename."
  :group 'org-ai
  :type '(choice (const :tag "No file" nil)
                 (file :tag "File")
                 (function :tag "Function")))

;; (setq org-ai-on-region-file (expand-file-name "org-ai-on-region.org" org-directory))

(cl-defun org-ai--output-to-buffer (start end text-prompt-fn output-buffer &optional &key show-output-buffer callback)
  "Get the currently selected text, create a prompt, insert the response.
`OUTPUT-BUFFER' is the buffer to insert the response in.
`TEXT-PROMPT-FN' is a function that takes the selected text as
argument and returns a prompt.
`START' is the buffer position of the region.
`END' is the buffer position of the region.
`OUTPUT-BUFFER' is the name or the buffer to insert the response in.
`CALLBACK' is a function to call after the response is inserted."
  (let* ((text (encode-coding-string (buffer-substring-no-properties start end) 'utf-8))
         (full-prompt (funcall text-prompt-fn text))
         (output-buffer (get-buffer-create output-buffer)))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (toggle-truncate-lines -1)
      (when show-output-buffer
        (display-buffer output-buffer)))
    (org-ai-prompt full-prompt :output-buffer output-buffer :callback callback)))

(declare-function org-clock-drawer-name "org-clock")
(declare-function org-ai-complete-block "org-ai")

(defun org-ai--insert-created-timestamp ()
  "Add a LOGBOOK entry CREATED with inactive timestamp."
  (require 'org-clock)
  (org-insert-drawer nil (org-clock-drawer-name))
  (insert "CREATED: ")
  (insert "[" (format-time-string "%F %a %R") "]")
  (forward-line -1)
  (org-indent-drawer)
  (forward-line 3))

(cl-defun org-ai--output-to-org-buffer (start end text-prompt-fn output-buffer &optional &key show-output-buffer)
  "Get the currently selected text, create a prompt, insert the response.
`OUTPUT-BUFFER' is the buffer to insert the response in.
`TEXT-PROMPT-FN' is a function that takes the selected text as
argument and returns a prompt.
`START' is the buffer position of the region.
`END' is the buffer position of the region.
`OUTPUT-BUFFER' is the name or the buffer to insert the response in.
`CALLBACK' is a function to call after the response is inserted."
  (let* ((text (buffer-substring-no-properties start end))
         (link (org-store-link '(4)))
         (full-prompt (funcall text-prompt-fn text))
         (output-buffer (or (get-buffer output-buffer)
                            (with-current-buffer (get-buffer-create output-buffer)
                              (org-mode)
                              (setq truncate-lines nil)
                              (setq word-wrap t)
                              (current-buffer)))))

    (when show-output-buffer
      (select-window (display-buffer output-buffer)))

    (with-current-buffer output-buffer
      (goto-char (point-max))
      (switch-to-buffer output-buffer)

      (unless (bobp) (insert "\n\n"))
      (insert "** ")
      (if link
          (insert link)
        (insert "*org-ai-on-region*"))
      (insert "\n")
      (org-ai--insert-created-timestamp)
      (insert "\n")
      (insert "#+begin_ai\n" "[ME]: " full-prompt "\n" "#+end_ai\n")
      (forward-line -1)
      (org-ai-complete-block))))


(defun org-ai--prefix-lines (str prefix)
  "Prepend all lines in `STR' with `PREFIX'."
  (replace-regexp-in-string "^" prefix str))

(defun org-ai--prompt-on-region-create-text-prompt (user-input text)
  "Create a prompt for `org-ai-on-region'.
`USER-INPUT' is the user input like a question to answer.
`TEXT' is the text of the region."
  (format "In the following I will show you a question and then a text. I want you to answer that question based on the text. Use the text as primary source but also add any external information you think is relevant.

Here is the question:
%s

Here is the text:
%s
" (org-ai--prefix-lines user-input "    ") (org-ai--prefix-lines text "    ")))

(defun org-ai--prompt-on-region-create-code-prompt (user-input code)
  "Create a prompt for `org-ai-on-region'.
`USER-INPUT' is the user input like a question to answer.
`CODE' is the code of the region."
  (format "In the following I will show you a question and then a code snippet. I want you to answer that question based on the code snippet.

Here is the question:
%s

Here is the code snippet:
%s
" user-input code))

(defun org-ai-on-region (start end question &optional buffer-name text-kind)
  "Ask ChatGPT to answer a question based on the selected text.
`QUESTION' is the question to answer.
`START' is the buffer position of the region.
`END' is the buffer position of the region.
`BUFFER-NAME' is the name of the buffer to insert the response in.
`TEXT-KIND' is either the symbol 'text or 'code. If nil, it will
be guessed from the current major mode."
  (interactive
   (let ((question (unless org-ai-talk-spoken-input (read-string "What do you want to know? " nil 'org-ai-on-region-history))))
     (list (region-beginning) (region-end) question)))

  (org-ai-with-input-or-spoken-text "What do you want to know?" question
    (let* ((text-kind (or text-kind (cond ((derived-mode-p 'prog-mode) 'code)
                                          ((derived-mode-p 'text-mode) 'text)
                                          (t 'text))))
           (text-prompt-fn (pcase text-kind
                             ('text (lambda (text) (org-ai--prompt-on-region-create-text-prompt question text)))
                             ('code (lambda (text) (org-ai--prompt-on-region-create-code-prompt question text)))
                             (_ (error "Invalid text-kind: %s" text-kind))))
           (output-buffer (let ((file (if (functionp org-ai-on-region-file)
                                          (funcall org-ai-on-region-file)
                                        org-ai-on-region-file)))
                            (or buffer-name
                                (when file (find-file-noselect file))
                                "*org-ai-on-region*"))))
      (org-ai--output-to-org-buffer start end text-prompt-fn output-buffer
                                    :show-output-buffer t
                                    ;; :callback (lambda ()
                                    ;;             (when org-ai-on-region-file
                                    ;;               (save-buffer)))
                                    ))))

(defcustom org-ai-summarize-prompt "Summarize this text."
  "The template to use for `org-ai-summarize'."
  :type 'string
  :group 'org-ai)

(defun org-ai-summarize (start end)
  "Ask ChatGPT for a summary of the marked text.
`START' is the buffer position of the start of the text to summarize.
`END' is the buffer position of the end of the text to summarize."
  (interactive "r")
  (org-ai-on-region start end org-ai-summarize-prompt nil 'text))

(defcustom org-ai-explain-code-prompt "The following shows a source code snippet. Explain what it does and mention potential issues and improvements."
  "The template to use for `org-ai-explain-code'."
  :type 'string
  :group 'org-ai)

(defun org-ai-explain-code (start end)
  "Ask ChatGPT explain a code snippet.
`START' is the buffer position of the start of the code snippet.
`END' is the buffer position of the end of the code snippet."
  (interactive "r")
  (org-ai-on-region start end org-ai-explain-code-prompt))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; refactor code

(defun org-ai-refactor-code (start end how)
  "Ask ChatGPT refactor a piece of code.
`START' is the buffer position of the start of the code snippet.
`END' is the buffer position of the end of the code snippet.
`HOW' is a string describing how the code should be modified."

  (interactive
   (let ((how (unless org-ai-talk-spoken-input (read-string "How should the code be modified? " nil 'org-ai-on-region-history))))
     (list (region-beginning) (region-end) how)))

  (org-ai-with-input-or-spoken-text "How should the code be modified? " how
    (let ((text-prompt-fn (lambda (code) (format "
In the following I will show you an instruction and then a code snippet. I want you to modify the code snippet based on the instruction. Only output the modified code. Do not include any explanation.

Here is the instruction:
%s

Here is the code snippet:
```
%s
```
" how code)))
          (buffer-with-selected-code (current-buffer))
          (file-name (buffer-file-name))
          (output-buffer (get-buffer-create "*org-ai-refactor*"))
          (win-config (current-window-configuration)))
      (org-ai--output-to-buffer start end text-prompt-fn output-buffer
                                  :show-output-buffer t
                                  :callback (lambda ()
                                              (progn
                                                (with-current-buffer output-buffer
                                                  ;; ensure buffer ends with a newline
                                                  (goto-char (point-max))
                                                  (unless (eq (char-before) ?\n) (insert ?\n))
                                                  ;; mark the whole buffer
                                                  (push-mark)
                                                  (push-mark (point-max) nil t)
                                                  (goto-char (point-min)))
                                                (org-ai--diff-and-patch-buffers buffer-with-selected-code output-buffer file-name)
                                                (set-window-configuration win-config)))))))

(defun org-ai--diff-and-patch-buffers (buffer-a buffer-b &optional file-name)
  "Will diff `BUFFER-A' and `BUFFER-B' and and offer to patch'.
`BUFFER-A' is the first buffer.
`BUFFER-B' is the second buffer.
`FILE-NAME' is the optional name of the file to use in the diff buffer header.
Will open the diff buffer and return it."
  (let* ((reg-A (with-current-buffer buffer-a
                  (cons (region-beginning) (region-end))))
         (reg-B (with-current-buffer buffer-b
                  (cons (region-beginning) (region-end))))
         (text-a (with-current-buffer buffer-a
                   (buffer-substring-no-properties (car reg-A) (cdr reg-A))))
         (text-b (with-current-buffer buffer-b
                   (buffer-substring-no-properties (car reg-B) (cdr reg-B))))
         (win-config (current-window-configuration))
         (diff-buffer (org-ai--diff-strings text-a text-b)))
    (when file-name
      (with-current-buffer diff-buffer
        (org-ai--diff-rename-files file-name file-name)))
    ;; Normally the diff would popup a new window. That's annoying.
    (set-window-configuration win-config)
    (display-buffer-use-some-window (get-buffer-create "*Diff*") nil)
    (prog1
     (when (y-or-n-p "Patch?")
       (pop-to-buffer buffer-a)
       (delete-region (car reg-A) (cdr reg-A))
       (insert text-b)
       (deactivate-mark)
       t)
     (kill-buffer diff-buffer)
     (kill-buffer buffer-b)
     (set-window-configuration win-config))))

(defun org-ai--diff-rename-files (file-name-a file-name-b &optional diff-header-start)
  "Will rename the files of the first file block of a diff buffer.
`FILE-NAME-A' is the name of the first file.
`FILE-NAME-B' is the name of the second file.
`DIFF-HEADER-START' is the start of the diff header, defaults to \"diff -u \".

E.g. will rename file-a.txt and file-b.txt to the specified names.
    diff -u file-a.txt file-b.txt
    --- file-a.txt	2023-04-17 01:48:47
    +++ file-b.txt	2023-04-17 01:48:47
Note: This expects only hunks of a single file."
  (let ((diff-header-start (or diff-header-start "diff -u "))
        (inhibit-read-only t))
    (save-excursion
      (let (start file-name-1 file-name-2)
        (goto-char (point-min))
        (search-forward diff-header-start)

        (setq start (point))
        (search-forward " ")
        (setq file-name-1 (buffer-substring start (- (point) 1)))
        (setq file-name-2 (buffer-substring (point) (line-end-position)))
        (list file-name-1 file-name-2)

        (goto-char (point-min))
        (while (search-forward file-name-1 nil t)
          (replace-match file-name-a))
        (goto-char (point-min))
        (while (search-forward file-name-2 nil t)
          (replace-match file-name-b))))))

;; (let ((buffer-with-selected-code (current-buffer))
;;       (output-buffer (get-buffer-create "*org-ai-refactor*")))
;;   (with-current-buffer output-buffer
;;     ;; ensure buffer ends with a newline
;;     (end-of-buffer)
;;     (unless (eq (char-before) ?\n) (insert ?\n))
;;     (mark-whole-buffer))
;;   (org-ai--diff-and-patch-buffers buffer-with-selected-code output-buffer))

(defun org-ai--diff-strings (string-a string-b)
  "Will create a unified diff of the two strings.
`STRING-A' is the first string.
`STRING-B' is the second string.
Will open the diff buffer and return it."
  (with-temp-buffer
    (insert string-a)
    (let ((diff-switches "-u")
          (temp-buffer-a (current-buffer)))
      (with-temp-buffer
        (insert string-b)
        (let* ((win (diff temp-buffer-a (current-buffer) nil t))
               (diff-buffer (window-buffer win)))
          diff-buffer)))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Marking blocks

(defun org-ai-mark-block-contents ()
  "Set the marked region to the contents of this org-ai block.
Place point at beginning of contents."
  (interactive)
  (when-let* ((block-info (cadr (org-ai-special-block)))
              (contents-begin (plist-get block-info :contents-begin))
              (contents-end (plist-get block-info :contents-end)))
    (goto-char contents-begin)
    (set-mark contents-end)
    (activate-mark)))

(defun org-ai-mark-block-after-point ()
  "Set the marked region to the contents of the org-ai block after point."
  (interactive)
  (when-let* ((block-info (cadr (org-ai-special-block)))
              (contents-end (plist-get block-info :contents-end)))
    (set-mark (- contents-end 1))
    (activate-mark)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-useful)

;;; org-ai-useful.el ends here
