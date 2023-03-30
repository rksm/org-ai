;;; org-ai.el --- Emacs org-mode integration for the OpenAI API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Robert Krahn

;; Author: Robert Krahn <robert@kra.hn>
;; URL: https://github.com/rksm/org-ai
;; Version: 0.2.1
;; Package-Requires: ((emacs "28.2"))

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
;; OpenAI API. It integrates ChatGPT and DALL-E into org-mode.
;; For more information, see https://github.com/rksm/org-ai.

;;; Code:

(require 'org-ai-block)
(require 'org-ai-openai)
(require 'org-ai-openai-image)
(require 'org-ai-useful)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun org-ai-ctrl-c-ctrl-c ()
  "This is added to `org-ctrl-c-ctrl-c-hook' to enable the `org-mode' integration."
  (when-let ((context (org-ai-special-block)))
    (org-ai-complete-block)
    t))

(defun org-ai-complete-block ()
  "Main command which is normally bound to \\[org-ai-complete-block].
When you are inside an #+begin_ai...#+end_ai block, it will send
the text content to the OpenAI API and replace the block with the
result."
  (interactive)
  (let* ((context (org-ai-special-block))
         (content (org-ai-get-block-content context))
         (info (org-ai-get-block-info context))
         (req-type (org-ai--request-type info))
         (sys-prompt-for-all-messages (or (not (eql 'x (alist-get :sys-everywhere info 'x)))
                                          org-ai-default-inject-sys-prompt-for-all-messages)))
    (cl-case req-type
      (completion (org-ai-stream-completion :prompt (encode-coding-string content 'utf-8)
                                            :context context))
      (image (org-ai-create-and-embed-image context))
      (t (org-ai-stream-completion :messages (org-ai--collect-chat-messages content org-ai-default-chat-system-prompt sys-prompt-for-all-messages)
                                   :context context)))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; keyboard quit

(defvar org-ai-talk--reading-process)

(defun org-ai-keyboard-quit ()
  "Keyboard quit advice.
It's designed to \"do the right thing\":
- If there is an active region, do nothing (normal \\<mapvar> & \\[keyboard-quit] will deactivate it).
- If there is speech recorded or played, stop it.
- If there is currently a running openai request, stop it."
  (interactive)
  (condition-case _
      (cond
       ((region-active-p) nil)
       ((and (boundp 'org-ai-talk--reading-process)
             (fboundp 'org-ai-talk-stop)
             org-ai-talk--reading-process
             (process-live-p org-ai-talk--reading-process))
        (org-ai-talk-stop))
       (org-ai--current-request-buffer
        (org-ai-interrupt-current-request)))
    (error nil)))

(defun org-ai--install-keyboard-quit-advice ()
  "Cancel current request when `keyboard-quit' is called."
  (unless (advice-member-p #'org-ai-keyboard-quit 'keyboard-quit)
    (advice-add 'keyboard-quit :before #'org-ai-keyboard-quit)))

(defun org-ai--uninstall-keyboard-quit-advice ()
  "Remove the advice that cancels current request when `keyboard-quit' is called."
  (advice-remove 'keyboard-quit #'org-ai-keyboard-quit))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar org-ai-mode-map (make-sparse-keymap)
  "Keymap for `org-ai-mode'.")

(let ((map org-ai-mode-map))
  (define-key map (kbd "C-c M-a v") 'org-ai-image-variation)
  (define-key map (kbd "C-c M-a $") 'org-ai-open-account-usage-page)
  (define-key map (kbd "C-c M-a SPC") 'org-ai-mark-region-at-point)
  (define-key map (kbd "C-c DEL") 'org-ai-kill-region-at-point)
  (define-key map (kbd "C-c <backspace>") 'org-ai-kill-region-at-point)
  (define-key map (kbd (string-join (list "C-c" " r"))) 'org-ai-talk-capture-in-org)) ; C-c r ¯\_(ツ)_/¯

;; create a minor-mode for org-mode
(define-minor-mode org-ai-mode
  "Minor mode for `org-mode' integration with the OpenAI API."
        :init-value nil
        :lighter " org-ai"
        :keymap org-ai-mode-map
        :group 'org-ai
        (add-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-ctrl-c-ctrl-c nil t))

(org-ai--install-keyboard-quit-advice)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defvar org-ai-global-mode-map (make-sparse-keymap)
  "Keymap for `org-ai-global-mode'.")

(let ((map org-ai-global-mode-map))
  (define-key map (kbd "C-c M-a p") 'org-ai-prompt)
  (define-key map (kbd "C-c M-a r") 'org-ai-on-region)
  (define-key map (kbd "C-c M-a c") 'org-ai-refactor-code)
  (define-key map (kbd "C-c M-a s") 'org-ai-summarize)
  (define-key map (kbd "C-c M-a m") 'org-ai-switch-chat-model)

  (define-key map (kbd "C-c M-a !") 'org-ai-open-request-buffer)
  (define-key map (kbd "C-c M-a $") 'org-ai-open-account-usage-page)
  (define-key map (kbd "C-c M-a t") 'org-ai-talk-input-toggle)
  (define-key map (kbd "C-c M-a T") 'org-ai-talk-output-toggle))

;;;###autoload
(define-minor-mode org-ai-global-mode
  "Non `org-mode' specific minor mode for the OpenAI API."
        :init-value nil
        :lighter " org-ai-global"
        :global t
        :keymap org-ai-global-mode-map
        :group 'org-ai)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai)

;;; org-ai.el ends here
