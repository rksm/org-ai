;;; org-ai-on-project.el --- Run org-ai on multiple files / a project  -*- lexical-binding: t; -*-

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

;; Run org-ai in the scope of a project.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'spinner)
(require 'org-ai-useful)

(eval-when-compile
  (require 'wid-edit))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; data structures for representing the selected project files

(cl-defstruct org-ai-on-project--state
  "Entire state of project in the on-project buffer."
  files
  base-dir
  pattern
  modify-code
  prompt)

(cl-defstruct org-ai-on-project--file
  "Represents a selected file choosen to be run on."
  file
  full-path
  region
  chosen)

(cl-defstruct org-ai-on-project--request-in-progress
  "State of the request in progress."
  state
  start-pos
  url-response-buffer
  spinner)

(defvar-local org-ai-on-project--current-state nil
  "Current state of the on-project buffer.")

(defvar org-ai-on-project--buffer-name "*org-ai-on-project*"
  "Name of the on-project buffer.")

(defvar org-ai-on-project--current-request-in-progress nil "")

(defun my-toggle-spinner ()
  (interactive)
  (if my-spinner
      (progn
        (spinner-stop my-spinner)
        (setq my-spinner nil))
    (setq my-spinner (spinner-create 'progress-bar t))
    (spinner-start my-spinner)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; helpers
(defun org-ai-on-project--reload ()
  (when (buffer-live-p (get-buffer org-ai-on-project--buffer-name))
    (with-current-buffer org-ai-on-project--buffer-name
      (let ((pos (point)))
        (org-ai-on-project--render org-ai-on-project--current-state)
        (goto-char pos)))))

(defun org-ai-on-project--find-files (base-dir patterns)
  (let* ((default-directory base-dir)
         (patterns (string-split patterns " " t))
         (known-project-files (or
                               (and (boundp 'projectile-mode)
                                    projectile-mode
                                    (fboundp #'projectile-current-project-files)
                                    (projectile-current-project-files)))))

    ;; This is quite a hack here:
    ;; If we have a pattern like "**/*js", it will only search in subdirectories,
    ;; not the base directory.
    ;; This is unlikely what the user want so we will add a pattern without the
    ;; "**/" prefix like "*js"
    (setq patterns (append patterns
                           (cl-loop for pattern in patterns
                                    with modified-pattern = nil
                                    when (string-prefix-p "**/" pattern)
                                    do (setq modified-pattern (string-replace "**/" "" pattern))
                                    when (not (member modified-pattern patterns))
                                    collect (string-replace "**/" "" pattern))))

    (cl-loop with found-files = nil
             for pattern in patterns
             do (cl-loop for file in (file-expand-wildcards pattern)
                         when (and (file-regular-p file)
                                   (not (string-match-p "__org-ai__" file))
                                   (or (not known-project-files)
                                       (member file known-project-files)))
                         do (push (make-org-ai-on-project--file
                                   :file file
                                   :full-path (expand-file-name file base-dir)
                                   :chosen t)
                                  found-files))
             finally return (nreverse found-files))))

(defun org-ai-on-project--get-file-content (file)
  "Read the content of FILE and return it as a string."
  (declare (indent 1))
  (let* ((full-path (org-ai-on-project--file-full-path file))
          (region (org-ai-on-project--file-region file))
          (region-start (car region))
          (region-end (cadr region))
          start end)
     (with-temp-buffer
       (insert-file-contents full-path)
       (goto-char (or region-start (point-min)))
       (beginning-of-line)
       (setq start (point))
       (goto-char (or region-start (point-max)))
       (end-of-line)
       (setq end (point))
       (buffer-substring-no-properties start end))))

(defun org-ai-on-project--extract-files-and-code-blocks (&optional start end)
  "Expects that the current buffer shows files and code.
This should be in the form:

file-name
```
code
```
...

`START' and `END' are optional and can be used to limit the
region.

Will return a hash table with the file names as keys and the
code as values."
  (let ((content (buffer-substring-no-properties (or start (point-min))
                                                 (or end (point-max))))
                 (result (make-hash-table :test 'equal)))
        (with-temp-buffer
          (insert content)
          (beginning-of-buffer)
          (cl-loop while (search-forward "```" nil t)
                   ;; sanity check, is this a code block, not embedded?
                   when (= (current-column) 3)
                   do (let (file-name file-content)
                        ;; we are at the end of the code block start
                        ;; get the file name
                        (beginning-of-line)
                        (forward-line -1)
                        (setq file-name (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position)))
                        (forward-line 2)
                        (let ((content-start (point)))
                          (search-forward "```")
                          (setq file-content (buffer-substring-no-properties
                                              content-start
                                              (line-beginning-position))))
                        (puthash file-name file-content result)))
          result)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; helper mode and functions for selecting regions in files

(defvar-local org-ai-on-project--select-region-file-already-open nil)
(defvar-local org-ai-on-project--select-region-file nil
  "File of type `org-ai-on-project--file` that is currently being selected.")

(define-minor-mode org-ai-on-project--select-region-mode
  "A mode for temporarily selecting a region in a file."
  :lighter " Select-Region"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'org-ai-on-project--confirm-selection)
            (define-key map (kbd "C-c k") (lambda () (interactive) (org-ai-on-project--confirm-selection t)))
            map))

(defun org-ai-on-project--confirm-selection (&optional cancel)
  "Confirm the selected region and stop `org-ai-on-project--select-region-mode`."
  (interactive)
  (when org-ai-on-project--select-region-mode
    (let ((region (when (and (not cancel) (region-active-p)) (list (region-beginning) (region-end))))
          (file org-ai-on-project--select-region-file))
      (message "Selected region: %s" region)
      (deactivate-mark)
      (org-ai-on-project--select-region-mode -1)
      (if org-ai-on-project--select-region-file-already-open
          (bury-buffer)
        (kill-this-buffer))
      (when file (setf (org-ai-on-project--file-region file) region))
      (org-ai-on-project--reload)
      (switch-to-buffer org-ai-on-project--buffer-name)
      (message "...%s" (current-buffer)))))

(defun org-ai-on-project--select-region-in-file (file)
  "Open FILE and start `org-ai-on-project--select-region-mode` to select a region.
FILE is of type `org-ai-on-project--file`. It is not a string!"
  (interactive "fSelect file: ")

  (let* ((file-path (org-ai-on-project--file-full-path file))
         (buf (if-let ((buf (find-buffer-visiting file-path)))
                 (with-current-buffer buf
                   (setq org-ai-on-project--select-region-file-already-open t)
                   buf)
               (find-file-literally file-path)
               (setq org-ai-on-project--select-region-file-already-open nil)
               (current-buffer))))
    (with-current-buffer buf
      (setq org-ai-on-project--select-region-file file)
      (org-ai-on-project--select-region-mode 1)
      (message "Select a region then press `C-c C-c'. Cancel with `C-c k'."))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; "UI" for the on-project buffer

(defmacro with-on-project-buffer (&rest body)
  `(progn
     (kill-buffer (get-buffer-create org-ai-on-project--buffer-name))
     (switch-to-buffer (get-buffer-create org-ai-on-project--buffer-name))
     (kill-all-local-variables)
     (with-current-buffer org-ai-on-project--buffer-name
       (toggle-truncate-lines -1)
       ,@body
       (use-local-map widget-keymap)
       (widget-setup)
       (org-ai-on-project-mode 1)
       (beginning-of-buffer))))

(defun org-ai-on-project--render (state)
  (let ((base-dir (org-ai-on-project--state-base-dir state))
        (files (org-ai-on-project--state-files state))
        (pattern (org-ai-on-project--state-pattern state))
        (prompt (org-ai-on-project--state-prompt state)))

    (with-on-project-buffer
      (setq org-ai-on-project--current-state state)

      (widget-insert "On project: " base-dir "\n\n")

      (widget-create 'text
                     :format "Prompt: %v"
                     :notify (lambda (widget &rest ignore)
                               (setf (org-ai-on-project--state-prompt state)
                                     (widget-value widget)))
                     (or (org-ai-on-project--state-prompt state) ""))

      (widget-insert "\n\n")

      (widget-create 'editable-field
		     :size 70
		     :format "Files: %v " ; Text after the field!
		     pattern)

      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (widget-backward 1)
                               (let* ((pos (point))
                                      (pattern (widget-value (widget-at (point))))
                                      (files (org-ai-on-project--find-files base-dir pattern)))
                                 (org-ai-on-project--render
                                  (make-org-ai-on-project--state :files files
                                                                 :base-dir base-dir
                                                                 :pattern pattern
                                                                 :prompt prompt))
                                 (goto-char pos)))
                     "Search")
      (widget-insert "\n\n")

      (cl-loop for file in files
               do (lexical-let ((file file))
                    (widget-create 'checkbox
                                   :notify (lambda (widget &rest ignore)
                                             (setf (org-ai-on-project--file-chosen file) (widget-value widget)))
                                   (org-ai-on-project--file-chosen file))
                    (widget-insert " " (org-ai-on-project--file-file file) " ")
                    (widget-create 'push-button
                                   :notify (lambda (&rest ignore)
                                             (org-ai-on-project--select-region-in-file file))
                                   (if-let ((region (org-ai-on-project--file-region file)))
                                       (format "%s-%s" (car region) (cadr region))
                                     "entire file"))
                    (widget-insert "\n")))

      (widget-insert "\n\n")

      (widget-insert "Modify code: ")
      (widget-create 'checkbox
                     :notify (lambda (widget &rest ignore)
                               (setf (org-ai-on-project--state-modify-code state)
                                     (widget-value widget)))
                     (org-ai-on-project--state-modify-code state))

      (widget-insert "\n\n")

      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (org-ai-on-project--run state))
                     "OK")

      (widget-insert " ")

      (widget-create 'push-button
                     :notify (lambda (&rest ignore) (kill-buffer))
                     "Cancel")
      )))

(defun org-ai-on-project--self-insert-command (N)
  "Helper for buffer commands."
  (interactive "p")
  (if (eq 'editable-field (widget-type (widget-at (point))))
      (self-insert-command N)
    (kill-buffer)))

(defvar org-ai-on-project-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")
                (lambda (N)
                  (interactive "p")
                  (if (eq 'editable-field (widget-type (widget-at (point))))
                      (self-insert-command N)
                    (kill-buffer))))
    (define-key map (kbd "g")
                (lambda (N)
                  (interactive "p")
                  (if (or (eq 'editable-field (widget-type (widget-at (point))))
                          (eq 'text (widget-type (widget-at (point)))))
                      (self-insert-command N)
                    (org-ai-on-project--reload))))
    map))

(define-minor-mode org-ai-on-project-mode
  "Minor mode for org-ai-on-project."
  :lighter " org-ai-on-project"
  :keymap org-ai-on-project-mode-map
  :group 'org-ai-on-project)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defcustom org-ai-on-project-default-prompt
  "In the following, I will show you a request and a list of file names together with their content. The files belong to the same project. I want you to answer the request using the file contents.

Here is the request:
I want you to <ENTER YOUR REQUEST HERE>

Here are the files:"
  "Default prompt for org-ai-on-project."
  :type 'string
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-default-modify-prompt
  "Now modify the code according to the request. Show it in the same format, file name followed by content. Leave out any files that you did not modify. You can add new files or split existing ones if necessary. Do not at any explanation whatsoever."
  "Default prompt for org-ai-on-project."
  :type 'string
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-default-request-prompt
  "Now answer the request using the file contents."
  "Default prompt for org-ai-on-project."
  :type 'string
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-use-stream t
  "If non-nil, use streaming to get the result.")

(defun org-ai-on-project--run (state)
  ""
  (let ((buf (get-buffer-create "*org-ai-on-project-result*"))
        (final-instruction (if (org-ai-on-project--state-modify-code state)
                               org-ai-on-project-default-modify-prompt
                             org-ai-on-project-default-request-prompt))
        (files (cl-loop for file in (org-ai-on-project--state-files state)
                        when (org-ai-on-project--file-chosen file)
                        collect file)))
    (with-current-buffer buf
      (toggle-truncate-lines -1)
      (erase-buffer)
      (insert (org-ai-on-project--state-prompt state))
      (insert "\n")
      (cl-loop for file in files
               do (let ((content (org-ai-on-project--get-file-content file))
                        (file (org-ai-on-project--file-file file)))
                    (insert file "\n")
                    (insert "```\n")
                    (insert content)
                    (insert "```\n\n")))
      (insert final-instruction "\n\n")
    (switch-to-buffer buf)
    (recenter-top-bottom 1)

    (let* ((prompt (buffer-string))
           (buf (current-buffer))
           (response-buffer (if org-ai-on-project-use-stream
                                (org-ai-prompt prompt
                                               :output-buffer (current-buffer)
                                               :callback (lambda ()
                                                           (with-current-buffer buf
                                                             (org-ai-on-project--request-cleanup))))
                              (org-ai-chat-request
                               :messages (org-ai--collect-chat-messages prompt)
                               :model org-ai-default-chat-model
                               :callback (lambda (content role usage)
                                           (with-current-buffer buf
                                             (save-excursion
                                               (insert content))
                                             (org-ai-on-project--request-cleanup)))))))

      (setq org-ai-on-project--current-request-in-progress
            (make-org-ai-on-project--request-in-progress
             :state state
             :start-pos (point)
             :spinner (spinner-start 'progress-bar)
             :url-response-buffer response-buffer))))))

(defun org-ai-on-project--request-cleanup ()
  ""
  (when-let (current org-ai-on-project--current-request-in-progress)
    (spinner-stop)
    ;; (display-buffer (org-ai-on-project--request-in-progress-url-response-buffer current))
    (unless org-ai-on-project-use-stream
      (kill-buffer (org-ai-on-project--request-in-progress-url-response-buffer current)))
    ;;(setq org-ai-on-project--current-request-in-progress nil)
    ))



(comment
  org-ai-on-project--current-request-in-progress

  (with-current-buffer "*org-ai-on-project-result*"
    (let ((pos (org-ai-on-project--request-in-progress-start-pos org-ai-on-project--current-request-in-progress)))
      (goto-char pos)
      (let ((modified (org-ai-on-project--extract-files-and-code-blocks))
            (original-and-modified-files (make-hash-table :test 'equal)))
        (cl-loop for key being the hash-keys of modified
                 using (hash-values value)
                 ;; for each file, create a __org-ai__ file that contains the modified content
                 do (let ((modified-file (replace-regexp-in-string "\\(.*/\\)?\\(.*\\)" "\\1__org-ai__\\2" key)))
                      (puthash key modified-file original-and-modified-files)
                      (with-temp-file modified-file
                        (insert value))))
        (setq xxx original-and-modified-files))))


  (hash-table-keys xxx)
  (gethash "test.js" xxx)

  (ediff-merge-files "test.js" "__org-ai__test.js")
  (setq file "foo/bar/baz.org")
  (setq file "baz.org"))




;;;###autoload
(defun org-ai-on-project (&optional base-dir)
  "Start org-ai-on-project.
This is a command that will allow you to run an org-ai prompt on
multiple files. You can select the files using a glob expression
and optionally select regions inside of the files.

Those files will then be concatenated and passed to org-ai with
your prompt."
  (interactive)
  (org-ai-on-project--render
   (make-org-ai-on-project--state :files nil
                                  :base-dir (or base-dir default-directory)
                                  :pattern "**/*"
                                  :prompt org-ai-on-project-default-prompt)))

;;; org-ai-on-project.el ends here
