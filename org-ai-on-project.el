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
(require 'wid-edit)
(require 'diff-mode)

(require 'org-ai-openai)
(require 'org-ai-useful)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; customizations

(defcustom org-ai-on-project-default-prompt
  "I will show you a request and a list of file names together with their content. The files belong to the same project. I want you to answer the request using the file contents.

Here is the request:
%s

The files are shown in the format
<FILENAME>
```
<FILE CONTENTS>
```

Here are the files:
"
  "Default prompt for `org-ai-on-project' in which to embed the users request."
  :type 'string
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-default-modify-prompt
  "Now modify the code according to the request. Show it in the same format, file name followed by content. Leave out any files that you did not modify. Do not add any explanation whatsoever. DO NOT ADD ANYTHING EXCEPT THE FILE NAMES AND THEIR CONTENTS!"
  "Default prompt for org-ai-on-project."
  :type 'string
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-modify-with-diff-prompt
  "Now modify the code according to the request. Skip any files that you did not modify. Output the modifications as valid unified diffs in the format:
<FILENAME>
```
--- <FILENAME>
+++ <FILENAME>
<CHANGES>
```
Ensure that the diff is valid. Do not add any explanation whatsoever. DO NOT ADD ANYTHING EXCEPT THE FILE NAMES AND THEIR CONTENTS!"
  "Default prompt for org-ai-on-project."
  :type 'string
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-modify-with-diffs
  nil
  "If non-nil, request that the model generate diffs.
This will be a lot faster because the model does not have to
replicate the entire file. But it also might lead to invalid
patches."
  :type 'boolean
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-default-request-prompt
  "Now answer the request using the file contents."
  "Default prompt for org-ai-on-project."
  :type 'string
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-use-stream t
  "If non-nil, use streaming to get the result."
  :type 'boolean
  :group 'org-ai-on-project)

(defcustom org-ai-on-project-max-files 300
  "The max number of files to show when searching."
  :type 'integer
  :group 'org-ai-on-project)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; data structures for representing the selected project files

(cl-defstruct org-ai-on-project--state
  "Entire state of project in the on-project buffer."
  base-dir
  file-search-pattern
  files
  org-ai-files
  modify-code
  modify-with-diffs
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
  url-response-buffer)

(defvar-local org-ai-on-project--last-state nil
  "Current state of the on-project buffer.")

(defvar org-ai-on-project--buffer-name "*org-ai-on-project*"
  "Name of the on-project buffer.")

(defvar org-ai-on-project--result-buffer-name "*org-ai-on-project-result*"
  "Name of the on-project result buffer.")

(defvar org-ai-on-project--current-request-in-progress nil)

(defvar org-ai-on-project--file-prefix ".orgai__"
  "Prefix used for files created by the responses.")

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; helpers

(defun org-ai-on-project--org-ai-file-for (file)
  "Return the org-ai file for FILE.
E.g. bar/foo.txt -> bar/.orgai__foo.txt."
  (replace-regexp-in-string
   "\\(.*/\\)?\\(.*\\)"
   (concat "\\1" org-ai-on-project--file-prefix "\\2")
   file))

(declare-function projectile-project-root "projectile")
(declare-function projectile-project-files "projectile")

(defun org-ai-on-project--find-files (base-dir regexp)
  "Find files in BASE-DIR matching REGEXP.
This uses projectile if it is available. Otherwise it restorts to
`directory-files-recursively'."
  (let (raw-files raw-org-ai-files)

    (if-let* ((project-dir (and (boundp 'projectile-mode)
                                projectile-mode
                                (fboundp #'projectile-project-root)
                                (projectile-project-root base-dir)))
              (files (and (fboundp #'projectile-project-files)
                          (projectile-project-files project-dir))))
        (progn
          (setq base-dir project-dir)
          (cl-loop for file in files
                   with org-ai-file-p
                   do (setq org-ai-file-p (string-match-p org-ai-on-project--file-prefix file))
                   when org-ai-file-p
                   do (push file raw-org-ai-files)
                   when (and (not org-ai-file-p)
                             (string-match-p regexp file))
                   do (push file raw-files)))

      (let ((default-directory base-dir))
        (setq raw-org-ai-files (cl-loop for file in (directory-files-recursively
                                                     "."
                                                     (concat (regexp-quote org-ai-on-project--file-prefix) ".*"))
                                        collect (string-remove-prefix "./" file)))
        (cl-loop for file in (directory-files-recursively "." ".*")
                 with exclude-regex = "\\(?:\\.git\\|\\.DS_Store\\)"
                 do (let ((file (string-remove-prefix "./" file)))
                      (when (and (not (string-match-p exclude-regex file))
                                 (string-match-p regexp file))
                        (push file raw-files))))))

    (cl-loop with found-files = nil
             with found-org-ai-files = (make-hash-table :test #'equal)

             for file in raw-files

             do (let ((org-ai-file (org-ai-on-project--org-ai-file-for file)))
                  (when (member org-ai-file raw-org-ai-files)
                    (puthash file org-ai-file found-org-ai-files)))

             do (push (make-org-ai-on-project--file
                       :file file
                       :full-path (expand-file-name file base-dir))
                      found-files)

             finally return (cons
                             (nreverse found-files)
                             (unless (hash-table-empty-p found-org-ai-files) found-org-ai-files)))))

(defun org-ai-on-project--do-search (state)
  "Find files matching the search pattern in STATE.
Find file modifications of previous prompts.
Add all those to STATE."
  (let* ((base-dir (org-ai-on-project--state-base-dir state))
         (pattern (org-ai-on-project--state-file-search-pattern state))
         (search-result (org-ai-on-project--find-files base-dir pattern)))
    (cl-destructuring-bind (files . org-ai-files) search-result
      (setf (org-ai-on-project--state-files state) files)
      (setf (org-ai-on-project--state-org-ai-files state) org-ai-files))))

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
      (goto-char (point-max))
      ;; ensure newline at end
      (unless (looking-back "\n" 1)
        (insert "\n")
        ;; ensure the original file has it as well, otherwise we can get issues
        ;; with patching
        (write-region (point-min) (point-max) full-path nil 'silent))
      (goto-char (or region-start (point-min)))
      (beginning-of-line)
      (setq start (point))
      (goto-char (or region-end (point-max)))
      (end-of-line)
      (setq end (point))
      (buffer-substring-no-properties start end))))

(defmacro org-ai-on-on-project--do-only-with-selected-file-content (file &rest body)
  "Helper macro to temporarily replace the content of FILE.
If file has a region set, put the content of that region into a
the file and run BODY. After BODY is done, restore the file and
move the potentially modified contents of file into that region.
If FILE has no region set, just run BODY in the context of the
file buffer."
  (declare (indent 1) (debug t))
  `(let* ((region (org-ai-on-project--file-region ,file))
          (full-path (org-ai-on-project--file-full-path ,file))
          (file-buffer (find-file-noselect full-path)))
     (if (not region)
         (with-current-buffer file-buffer ,@body)
       (let ((selected-content (org-ai-on-project--get-file-content ,file))
             (backup-buffer (get-buffer-create "*org-ai-on-project--patch-helper*")))
         (with-current-buffer backup-buffer
           (erase-buffer)
           (insert-file-contents (org-ai-on-project--file-full-path ,file)))
         (unwind-protect
             (with-current-buffer file-buffer
               (erase-buffer)
               (insert selected-content)
               (basic-save-buffer)
               ,@body
               (with-current-buffer file-buffer
                 (let (start
                       end
                       (new-content (buffer-string)))
                   (with-current-buffer backup-buffer
                     (goto-char (car region))
                     (beginning-of-line)
                     (setq start (point))
                     (goto-char (cadr region))
                     (end-of-line)
                     (setq end (point))
                     (delete-region start end)
                     (goto-char start)
                     (insert new-content)))))
           (with-current-buffer backup-buffer
             (let ((content (buffer-string)))
               (with-current-buffer file-buffer
                 (erase-buffer)
                 (insert content)
                 (basic-save-buffer)))
             (kill-buffer backup-buffer)))))))

(defun org-ai-on-project--extract-files-and-code-blocks (&optional start end expect-diffs)
  "Expects that the current buffer shows files and code.
This should be in the form:

file-name
```
code
```
...

`START' and `END' are optional and can be used to limit the
region.

`EXPECT-DIFFS' is optional and if set to t, will expect that
the model outputs a unified diff instead of verbatim code.

Will return a hash table with the file names as keys and the
code as values."
  (let ((content (buffer-substring-no-properties (or start (point-min))
                                                 (or end (point-max))))
        (result (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
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
                      (when expect-diffs
                        ;; if we parse a diff, we need to ensure that empty lines
                        ;; have a leading space, otherwise the patching will fail
                        (cl-loop while (and (not (looking-at "```")) (not (eobp)))
                                 do (let ((line-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                                      (when (string-equal line-string "")
                                        (insert " ")
                                        (forward-char -1))
                                      (forward-line 1))))
                      (search-forward "```")
                      (setq file-content (buffer-substring-no-properties
                                          content-start
                                          (line-beginning-position))))
                    (puthash file-name file-content result)))
      result)))

(defun org-ai-on-project--patch-file (state file)
  "Patch the file FILE in the project of STATE."
  (let* ((file-name (org-ai-on-project--file-file file))
         (org-ai-file (gethash file-name (org-ai-on-project--state-org-ai-files state)))
         (has-diff-p (org-ai-on-project--state-modify-with-diffs state))
         (buffer-a (find-file-noselect file-name))
         (buffer-b (find-file-noselect org-ai-file)))

    (if has-diff-p
        (let* ((win-config (current-window-configuration)))
          (org-ai-on-on-project--do-only-with-selected-file-content file
            (switch-to-buffer buffer-b)
            (diff-mode)
            (if-let ((err (condition-case err
                              (progn
                                (diff-fixup-modifs (point-min) (point-max))
                                (basic-save-buffer)
                                (diff-test-hunk)
                                nil)
                            (error err))))
                (run-with-idle-timer 0.1 nil
                                     (lambda ()
                                       (message "The patch is invalid: %s" err)))
              (progn
                (goto-char (point-min))
                (display-buffer buffer-a)
                (when (y-or-n-p "Apply diff? ")
                  (diff--iterate-hunks (point-max)
                                       (lambda (&rest _ignore) (diff-apply-hunk)))
                  (org-ai-on-project--remove-org-ai-file state file-name org-ai-file))
                (kill-buffer buffer-b)
                (with-current-buffer buffer-a (basic-save-buffer))
                (set-window-configuration win-config)
                (org-ai-on-project--render state)))))

      (with-current-buffer buffer-a
        (if-let ((region (org-ai-on-project--file-region file)))
            (progn
              (goto-char (car region))
              (set-mark (cadr region)))
          (progn ;; mark-whole-buffer
            (push-mark)
            (push-mark (point-max) nil t)
            (goto-char (point-min)))))
      (with-current-buffer buffer-b (progn ;; mark-whole-buffer
                                      (push-mark)
                                      (push-mark (point-max) nil t)
                                      (goto-char (point-min))))
      (when (org-ai--diff-and-patch-buffers buffer-a buffer-b file-name)
        (with-current-buffer buffer-a (basic-save-buffer))
        (org-ai-on-project--remove-org-ai-file state file-name org-ai-file)
        (org-ai-on-project--render state)))))

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
            (define-key map (kbd (string-join (list "C-c" " C-k"))) (lambda () (interactive) (org-ai-on-project--confirm-selection t)))
            map))

(defun org-ai-on-project--confirm-selection (&optional cancel)
  "Confirm the selected region and stop `org-ai-on-project--select-region-mode`.
If CANCEL is non-nil, cancel the selection."
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
      (switch-to-buffer org-ai-on-project--buffer-name))))

(defun org-ai-on-project--select-region-in-file (file)
  "Open FILE and start `org-ai-on-project--select-region-mode` to select a region.
FILE is of type `org-ai-on-project--file`. It is not a string!"
  (interactive "fSelect file: ")

  (let* ((file-path (org-ai-on-project--file-full-path file))
         (region (org-ai-on-project--file-region file))
         (buf (if-let ((buf (find-buffer-visiting file-path)))
                  (with-current-buffer buf
                    (setq org-ai-on-project--select-region-file-already-open t)
                    buf)
                (find-file-literally file-path)
                (setq org-ai-on-project--select-region-file-already-open nil)
                (current-buffer))))
    (switch-to-buffer buf)
    (setq org-ai-on-project--select-region-file file)
    (org-ai-on-project--select-region-mode 1)
    (when region
      (goto-char (car region))
      (set-mark (cadr region)))
    (message "Select a region then press `C-c C-c'. Cancel with `C-c C-k'.")))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; "UI" for the on-project buffer

(defmacro org-ai-on-project--with-on-project-buffer (&rest body)
  "Ensure/setup `org-ai-on-project--buffer-name` and execute BODY."
  `(progn
     (kill-buffer (get-buffer-create org-ai-on-project--buffer-name))
     (switch-to-buffer (get-buffer-create org-ai-on-project--buffer-name))
     (kill-all-local-variables)
     (with-current-buffer org-ai-on-project--buffer-name
       (setq truncate-lines nil)
       (setq word-wrap t)
       ,@body
       (use-local-map widget-keymap)
       (widget-setup)
       (org-ai-on-project-mode 1))))

(defun org-ai-on-project--render (state)
  "Render STATE using widgets in `org-ai-on-project--buffer-name`."
  (let ((base-dir (org-ai-on-project--state-base-dir state))
        (has-modifications-p (org-ai-on-project--state-org-ai-files state))
        (pos (if-let ((buf (get-buffer org-ai-on-project--buffer-name))) (with-current-buffer buf (point)) 0)))

    (org-ai-on-project--with-on-project-buffer
     (setq org-ai-on-project--last-state state)
     (setq-local default-directory base-dir)

     (widget-create 'text :format (concat "%{On project: " base-dir "%}") :sample-face 'header-line)
     (widget-insert "\n\n")

     (org-ai-on-project--render-search-input state)
     (widget-insert "\n\n")
     (org-ai-on-project--render-files state)
     (widget-insert "\n\n\n")

     (widget-create 'text :format (concat "%{Prompt" "%}") :sample-face 'header-line)
     (widget-insert "\n\n")
     (org-ai-on-project--render-prompt state)
     (widget-insert "\n")

     ;; render controls
     (if has-modifications-p
         (org-ai-on-project--render-with-modification-controls state)
       (org-ai-on-project--render-without-modification-controls state))

     (widget-insert " ")

     (org-ai-on-project--render-quit state)

     (goto-char pos))))

(defun org-ai-on-project--render-prompt (state)
  "Render the input prompt.
STATE is `org-ai-on-project--state'."
  (widget-create 'text
                 :format "%v"
                 :notify (lambda (widget &rest _ignore)
                           (setf (org-ai-on-project--state-prompt state) (widget-value widget)))
                 (or (org-ai-on-project--state-prompt state) "")))

(defun org-ai-on-project--render-search-input (state)
  "Render the search input widget.
STATE is `org-ai-on-project--state'."
  (let ((pattern (org-ai-on-project--state-file-search-pattern state)))
    (widget-create 'editable-field :size 40 :format "Files: %v " pattern)
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
                             (widget-backward 1)
                             (let ((pos (point))
                                   (pattern (widget-value (widget-at (point))))
                                   (org-ai-files (org-ai-on-project--state-org-ai-files state)))
                               (setf (org-ai-on-project--state-file-search-pattern state) pattern)
                               (org-ai-on-project--do-search state)
                               (org-ai-on-project--render state)
                               (goto-char pos)
                               (when org-ai-files (message "Found existing set of .orgai__* files!"))))
                   "Search")))

(defun org-ai-on-project--render-files (state)
  "Render the files.
If we have modifications, offer diff/patch options.
STATE is `org-ai-on-project--state'."
  (let* ((files (org-ai-on-project--state-files state))
         (has-modifications-p (org-ai-on-project--state-org-ai-files state))
         (too-many-files-p (> (length files) org-ai-on-project-max-files)))
    (if has-modifications-p
        (cl-loop for file in files
                 do (org-ai-on-project--render-file-with-modification state file))
      (cl-loop for file in files
               do (org-ai-on-project--render-file-without-modification state file)))

    (when too-many-files-p
      (widget-insert (format "\nToo many files to display. Showing %s of %s files.\n"
                             org-ai-on-project-max-files
                             (length files)))))

  (widget-insert "\n")

  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (cl-loop with files = (org-ai-on-project--state-files state)
                                    for file in files
                                    do (setf (org-ai-on-project--file-chosen file) t))
                           (org-ai-on-project--render state))
                 "Select all")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (cl-loop with files = (org-ai-on-project--state-files state)
                                    for file in files
                                    do (setf (org-ai-on-project--file-chosen file) nil))
                           (org-ai-on-project--render state))
                 "Select none"))


(defun org-ai-on-project--render-file-without-modification (_state file)
  "Render FILE without modification using _STATE.
_STATE is `org-ai-on-project--state'.
FILE is `org-ai-on-project--file'."
  (let ((file-name (org-ai-on-project--file-file file))
        (chosen (org-ai-on-project--file-chosen file)))
    (widget-create 'checkbox
                   :notify (lambda (&rest _ignore)
                             (setf (org-ai-on-project--file-chosen file) (not chosen)))
                   chosen)
    (widget-insert " ")
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :button-face 'widget-field-face
                   :notify (lambda (&rest _ignore) (find-file file-name))
                   file-name)
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
                             (org-ai-on-project--select-region-in-file file))
                   (if-let ((region (org-ai-on-project--file-region file)))
                       (format "%s-%s" (car region) (cadr region))
                     "entire file"))
    (widget-insert "\n")))

(defun org-ai-on-project--render-file-with-modification (state file)
  "Render FILE with available modifications (org-ai-file).
STATE is `org-ai-on-project--state'.
FILE is `org-ai-on-project--file'."
  (let* ((file-name (org-ai-on-project--file-file file))
         (org-ai-files (org-ai-on-project--state-org-ai-files state))
         (org-ai-file (gethash file-name org-ai-files)))

    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :button-face 'widget-field-face
                   :notify (lambda (&rest _ignore) (find-file file-name))
                   file-name)
    (widget-insert " ")

    (when org-ai-file
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (find-file org-ai-file)
                               (when (org-ai-on-project--state-modify-with-diffs state)
                                 (diff-mode)))
                     "Show changes")
      (widget-insert " ")

      (widget-create 'push-button
                     :notify (lambda (&rest _ignore) (org-ai-on-project--patch-file state file))
                     "Patch")
      (widget-insert " ")

      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (org-ai-on-project--remove-org-ai-file state file-name org-ai-file)
                               (org-ai-on-project--render state)
                               (beginning-of-line))
                     "Reset"))

    (widget-insert "\n")))

(defun org-ai-on-project--render-without-modification-controls (state)
  "Render some controls.
STATE is `org-ai-on-project--state'."
  (widget-insert "Modify code: ")
  (widget-create 'checkbox
                 :notify (lambda (widget &rest _ignore)
                           (setf (org-ai-on-project--state-modify-code state)
                                 (widget-value widget)))
                 (org-ai-on-project--state-modify-code state))
  (widget-insert "\n")

  (widget-insert "Request diffs: ")
  (widget-create 'checkbox
                 :notify (lambda (&rest _ignore)
                           (setf (org-ai-on-project--state-modify-with-diffs state)
                                 org-ai-on-project-modify-with-diffs))
                 (org-ai-on-project--state-modify-with-diffs state))
  (widget-insert " (experimental)")
  (widget-insert "\n\n")

  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (org-ai-on-project--run state))
                 "Run"))

(defun org-ai-on-project--render-with-modification-controls (state)
  "Render some controls.
STATE is `org-ai-on-project--state'."
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (org-ai-on-project--remove-org-ai-files state)
                           (org-ai-on-project--run state))
                 "Run again")
  (widget-insert "\n\n")

  ;; not yet implemented
  ;; (widget-create 'push-button
  ;;                :notify (lambda (&rest _ignore) (org-ai-on-project--run state))
  ;;                "Diff all")
  ;; (widget-insert " ")
  ;; (widget-create 'push-button
  ;;                :notify (lambda (&rest _ignore) (org-ai-on-project--run state))
  ;;                "Patch all")
  ;; (widget-insert "\n\n")

  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (let ((org-ai-files (org-ai-on-project--state-org-ai-files state)))
                             (when (and org-ai-files (y-or-n-p "Discard changes?"))
                               (org-ai-on-project--remove-org-ai-files state)
                               (org-ai-on-project--render state)
                               (goto-char (point-min)))))
                 "Reset"))

(defun org-ai-on-project--render-quit (state)
  "Render a quit button.
STATE is `org-ai-on-project--state'."
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (let ((org-ai-files (org-ai-on-project--state-org-ai-files state)))
                             (when (or (not org-ai-files) (y-or-n-p "Discard changes?"))
                               (org-ai-on-project--remove-org-ai-files state)
                               (kill-buffer))))
                 "Quit"))

(defun org-ai-on-project--reload ()
  "Reload the on-project buffer."
  (when (buffer-live-p (get-buffer org-ai-on-project--buffer-name))
    (with-current-buffer org-ai-on-project--buffer-name
      (org-ai-on-project--render org-ai-on-project--last-state))))

(defun org-ai-on-project--self-insert-command (N)
  "Helper for buffer commands.
N is the number of times to repeat the command."
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

(add-hook 'org-ai-on-project-mode-hook
          (lambda () (setq-local show-trailing-whitespace nil)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun org-ai-on-project--run (state)
  "Takes the current STATE and runs the AI on it.
This will open a result
buffer (`org-ai-on-project--result-buffer-name') and insert the
full prompt as well as the result in there. Once this is done, we
either leave the result buffer around (no file modifications
requested) or we:
1. extract the modified file contents from the AI response,
2. create local files in the directory alongside the original
   files, prefixed with `org-ai-on-project--modified-file-prefix',
3. go back to the on-project
   buffer (`org-ai-on-project--buffer-name') and allow the user
   to inspect the changes."
  (let ((buf (get-buffer-create org-ai-on-project--result-buffer-name))
        (prompt (org-ai-on-project--state-prompt state))
        (final-instruction (if (org-ai-on-project--state-modify-code state)
                               (if (org-ai-on-project--state-modify-with-diffs state)
                                   org-ai-on-project-modify-with-diff-prompt
                                 org-ai-on-project-default-modify-prompt)
                             org-ai-on-project-default-request-prompt))
        (files (cl-loop for file in (org-ai-on-project--state-files state)
                        when (org-ai-on-project--file-chosen file)
                        collect file)))

    (unless files
      (error "No files selected"))
    (unless prompt
      (error "No prompt"))

    (with-current-buffer buf
      (setq-local default-directory (org-ai-on-project--state-base-dir state))
      (toggle-truncate-lines -1)
      (erase-buffer)

      ;; insert the full prompt and the selected files + their content
      (insert (format org-ai-on-project-default-prompt prompt))
      (insert "\n")
      (cl-loop for file in files
               do (let ((content (org-ai-on-project--get-file-content file))
                        (file (org-ai-on-project--file-file file)))
                    (insert file "\n")
                    (insert "```\n")
                    (insert content)
                    (unless (string-suffix-p "\n" content) (insert "\n"))
                    (insert "```\n\n")))
      (insert final-instruction "\n\n")
      (switch-to-buffer buf)
      (recenter-top-bottom 1))

    ;; now run the AI model on it
    (let* ((prompt (with-current-buffer buf (buffer-string)))
           (start-pos (with-current-buffer buf (point)))

           (response-buffer (if org-ai-on-project-use-stream
                                (org-ai-prompt prompt
                                               :follow t
                                               :output-buffer buf
                                               :callback (lambda ()
                                                           (when-let ((request (org-ai-on-project--request-cleanup)))
                                                             (org-ai-on-project--run-done request))))
                              (org-ai-chat-request
                               :messages (org-ai--collect-chat-messages prompt)
                               :model org-ai-default-chat-model
                               :callback (lambda (content _role _usage)
                                           (with-current-buffer buf
                                             (save-excursion (insert content))
                                             (when-let ((request (org-ai-on-project--request-cleanup)))
                                               (org-ai-on-project--run-done request)))))))

           (request (make-org-ai-on-project--request-in-progress
                     :state state
                     :start-pos start-pos
                     :url-response-buffer response-buffer)))

      (setq org-ai-on-project--current-request-in-progress request))))

(defun org-ai-on-project--request-cleanup ()
  "Cleans up the current `org-ai-on-project--current-request-in-progress'.
This will kill the url response buffer."
  (when-let (current org-ai-on-project--current-request-in-progress)
    ;; (display-buffer (org-ai-on-project--request-in-progress-url-response-buffer current))
    (unless org-ai-on-project-use-stream
      (kill-buffer (org-ai-on-project--request-in-progress-url-response-buffer current)))
    (setq org-ai-on-project--current-request-in-progress nil)
    current))

(defun org-ai-on-project--run-done (request)
  "Takes the current REQUEST and does the post-processing."
  (let ((pos (org-ai-on-project--request-in-progress-start-pos request))
        (state (org-ai-on-project--request-in-progress-state request)))
    (goto-char pos)

    ;; extract & write the modified files if the user requested that
    (when (org-ai-on-project--state-modify-code state)
      (let ((modified (org-ai-on-project--extract-files-and-code-blocks pos (point-max)
                                                                        (org-ai-on-project--state-modify-with-diffs state)))
            (original-and-modified-files (make-hash-table :test 'equal)))
        (cl-loop for key being the hash-keys of modified
                 using (hash-values value)
                 ;; for each file, create a .org-ai__ file that contains the modified content
                 do (let ((modified-file (org-ai-on-project--org-ai-file-for key)))
                      (puthash key modified-file original-and-modified-files)
                      (with-temp-file modified-file
                        (insert value))))
        (setf (org-ai-on-project--state-org-ai-files state) original-and-modified-files)
        (bury-buffer)
        (switch-to-buffer org-ai-on-project--buffer-name)
        (org-ai-on-project--render state)
        (goto-char (point-min))
        (search-forward "Patch" nil t)))))

(defun org-ai-on-project--remove-org-ai-files (state)
  "Remove all the .orgai__* files created by org-ai-on-project.
STATE is the current state."
  (when-let (org-ai-files (org-ai-on-project--state-org-ai-files state))
    (cl-loop for value being the hash-values of org-ai-files
             do (when (file-exists-p value)
                  (delete-file value)))
    (setf (org-ai-on-project--state-org-ai-files state) nil)
    t))

(defun org-ai-on-project--remove-org-ai-file (state orig-file org-ai-file)
  "Remove all the .orgai__* files created by org-ai-on-project.
ORIG-FILE is the original file that was modified.
ORG-AI-FILE is the .org-ai__ file that was created.
STATE is the current state."
  (when-let (org-ai-files (org-ai-on-project--state-org-ai-files state))
    (remhash orig-file org-ai-files)
    (when (file-exists-p org-ai-file)
      (delete-file org-ai-file))
    (when (hash-table-empty-p org-ai-files)
      (setf (org-ai-on-project--state-org-ai-files state) nil))
    t))


;;;###autoload
(defun org-ai-on-project (&optional base-dir)
  "Start org-ai-on-project inside BASE-DIR.
This is a command that will allow you to run an org-ai prompt on
multiple files. You can select the files using a regexp expression
and optionally select regions inside of the files.

Those files will then be concatenated and passed to org-ai with
your prompt."
  (interactive)
  (let ((dir (or base-dir default-directory)))
    (if-let* ((buf (get-buffer org-ai-on-project--buffer-name))
              (state (with-current-buffer buf org-ai-on-project--last-state))
              (last-dir (org-ai-on-project--state-base-dir state))
              (descendent-p (file-in-directory-p dir last-dir)))
        (progn
          (switch-to-buffer buf)
          (org-ai-on-project--render state))
      (let ((state (make-org-ai-on-project--state :base-dir dir
                                                  :modify-code t
                                                  :modify-with-diffs org-ai-on-project-modify-with-diffs
                                                  :file-search-pattern ".*"
                                                  :prompt (if org-ai-on-project--last-state
                                                              (org-ai-on-project--state-prompt org-ai-on-project--last-state)
                                                            ""))))
        (org-ai-on-project--do-search state)
        (org-ai-on-project--render state)))))


(provide 'org-ai-on-project)

;;; org-ai-on-project.el ends here
