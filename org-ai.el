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

(defvar org-ai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-a v") 'org-ai-image-variation)
    (define-key map (kbd "C-c M-a $") 'org-ai-open-account-usage-page)
    (define-key map (kbd "C-c M-a SPC") 'org-ai-mark-region-at-point)
    (define-key map (kbd "C-c k") 'org-ai-kill-region-at-point)
    map)
  "Keymap for `org-ai-mode'.")

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
