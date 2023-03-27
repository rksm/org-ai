;;; org-ai.el --- Emacs org-mode integration for the OpenAI API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Robert Krahn

;; Author: Robert Krahn <robert@kra.hn>
;; URL: https://github.com/rksm/org-ai
;; Version: 0.1.4
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

(defun org-ai-keyboard-quit ()
  "If there is currently a running request, cancel it."
  (interactive)
  (condition-case _
      (cond
       ((region-active-p) nil)
       ((and rk/org-ai-reading-process (process-live-p rk/org-ai-reading-process))
        (rk/org-ai-read-stop))
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
  (define-key map (kbd "C-c k") 'org-ai-kill-region-at-point))

;; create a minor-mode for org-mode
(define-minor-mode org-ai-mode
  "Toggle `org-ai-mode'."
        :init-value nil
        :lighter " org-ai"
        :keymap org-ai-mode-map
        :group 'org-ai
        (add-hook 'org-ctrl-c-ctrl-c-hook #'org-ai-ctrl-c-ctrl-c nil t))

(org-ai--install-keyboard-quit-advice)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai)

;;; org-ai.el ends here
