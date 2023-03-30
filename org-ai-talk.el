;;; org-ai-whisper.el --- Talk with your AI -*- lexical-binding: t; -*-

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

;; Speech-to-text-to-speech! Using whisper.el, espeak/greader and on macOS
;; internal speech synthesis.

;;; Code:

(require 'org-ai-useful)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; recording and transcribing speech

(defvar whisper--transcribing-process)
(defvar whisper--recording-process)
(defvar whisper--point-buffer)

(defvar org-ai-talk--whisper-transcription-buffer nil
  "The buffer into which the speech transcription is written.")

(defun org-ai-talk--record-and-transcribe-speech (then-do &optional prompt)
  "Record speech and call `THEN-DO' with the transcription as argument.
If `PROMPT' is non-nil, display it while recording.

NOTE: This requires whisper.el to be installed. Since whisper.el
is not part of MELPA, you have to install it manually. See
https://github.com/natrys/whisper.el for instructions."
  (if (fboundp 'whisper--check-install-and-run)
      (let* ((buffer (or org-ai-talk--whisper-transcription-buffer
                         (get-buffer-create "*org-ai-talk--whisper-transcription-buffer*"))))
        (setq whisper--point-buffer buffer)
        (setq org-ai-talk--whisper-transcription-buffer buffer)
        (whisper--check-install-and-run nil "whisper-start")
        (when prompt (message prompt))
        (while-no-input
          (let ((key (read-key-sequence "Press any key to stop recording, cancel with C-g or ESC...")))
            (org-ai-talk--stop-recording
             (if (or (equal key "")
                     (equal key "")
                     (equal key ""))
                 (lambda ()
                   (message "canceled recording")
                   (kill-buffer org-ai-talk--whisper-transcription-buffer)
                   (setq org-ai-talk--whisper-transcription-buffer nil))
               (lambda ()
                 (let ((content (with-current-buffer org-ai-talk--whisper-transcription-buffer
                                  (string-trim (buffer-string)))))
                   (funcall then-do content))
                 (kill-buffer org-ai-talk--whisper-transcription-buffer)
                 (setq org-ai-talk--whisper-transcription-buffer nil)))))))
    (error "The package whisper.el is not installed but needed for transcribing speech")))

;; (lexical-let ((test-buf (get-buffer-create "*test-buffer*")))
;;   (display-buffer-same-window test-buf)
;;   (org-ai-talk--record-and-transcribe-speech (lambda (spoken-text)
;;                                                (with-current-buffer test-buf
;;                                                  (insert spoken-text)
;;                                                  (insert "\n\n")))
;;                                              "Say something then press any key..."))

(defun org-ai-talk--stop-recording (then-do)
  "Force current recording to stop and call `THEN-DO'."
  (when (process-live-p whisper--recording-process)
    (interrupt-process whisper--recording-process))
  (run-with-timer
   0.1
   nil
   (lambda (then-do)
     (if (process-live-p whisper--transcribing-process)
         (run-with-timer 0.1
                         nil
                         (lambda (then-do) (org-ai-talk--stop-recording then-do))
                         then-do)
       (funcall then-do)))
   then-do))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; synthesizing speech

(defcustom org-ai-talk-say-words-per-minute nil
  "Speech rate passed to the `say' macOS cli command (`-r' flag).
Nil means use the default rate."
  :type 'integer
  :group 'org-ai-talk)

(defcustom org-ai-talk-say-voice nil
  "Voice passed to the `say' macOS cli command (`-v' flag).
Nil means use the default voice."
  :type 'integer
  :group 'org-ai-talk)

(defcustom org-ai-talk-use-greader
  (not (string-equal system-type "darwin"))
  "Use greader / espeak for speech synthesis?"
  :type 'boolean
  :group 'org-ai-talk)

(defvar org-ai-talk--current-insertion-point 0)
(defvar org-ai-talk--read-buffer nil)
(defvar org-ai-talk--reading-process nil)
(defvar org-ai-talk--is-done nil)

(defun org-ai-talk--read-buffer-alive-p ()
  "Return t if `org-ai-talk--read-buffer' is alive."
  (and org-ai-talk--read-buffer (buffer-live-p org-ai-talk--read-buffer)))

(defun org-ai-talk--ensure-read-buffer ()
  "Ensure `org-ai-talk--read-buffer' is set and return it."
  (when (not (org-ai-talk--read-buffer-alive-p))
    (message "creating buffer for speech synthesis")
    (setq org-ai-talk--read-buffer (get-buffer-create "*org-ai-talk*")))
  org-ai-talk--read-buffer)

(defun org-ai-talk--speak-inserted-text (type content)
  "Used with `org-ai-after-chat-insertion-hook'.
When installed, will speak the text that we get back from the AI.

`TYPE' is either 'role or 'text or 'end.
`CONTENT' is the text that was inserted."
  (cl-case type
    (role
     (setq org-ai-talk--current-insertion-point 0)
     (setq org-ai-talk--is-done nil)
     (with-current-buffer (org-ai-talk--ensure-read-buffer)
       (erase-buffer)))
    (text
     (if (org-ai-talk--read-buffer-alive-p)
         (with-current-buffer org-ai-talk--read-buffer
           (save-excursion (goto-char org-ai-talk--current-insertion-point)
                           (insert content)
                           (setq org-ai-talk--current-insertion-point (point)))
           ;; start speaking for sentences
           (when (and (not org-ai-talk--reading-process) (string-match-p "\\.\\|!\\|\\?\\|:\\|," content))
             (org-ai-talk--read-next-sentence (lambda ()
                                                (when org-ai-talk--is-done
                                                  (with-current-buffer org-ai-talk--read-buffer
                                                    (org-ai-talk-read-region (point) (point-max))))))))
       (setq org-ai-talk--is-done t)))

    (end
     (when (and (org-ai-talk--read-buffer-alive-p) (not org-ai-talk--reading-process))
       (with-current-buffer org-ai-talk--read-buffer
         (org-ai-talk-read-region (point) (point-max))))
     (setq org-ai-talk--is-done t))))


(defun org-ai-talk-read-region (from to &optional callback)
  "Read the region from `FROM' to `TO'.
If `CALLBACK' is non-nil, call it when done."
  (interactive "r")
  (when (and org-ai-talk--reading-process (process-live-p org-ai-talk--reading-process))
    (warn "already reading aloud")
    (kill-process org-ai-talk--reading-process))
  (org-ai-talk--ensure-read-buffer)
  (cond
   (org-ai-talk-use-greader
    (org-ai-talk--read-region-greader from to callback))
   ((string-equal system-type "darwin")
    (org-ai-talk--read-region-macos from to callback))
   (t
    (error "No speech synthesis available"))))

(defun org-ai-talk--read-region-greader (from to &optional callback)
  "Read the region from `FROM' to `TO'.
If `CALLBACK' is non-nil, call it when done.
Uses greader / espeak, should work on all platforms where espeak is installed."
  (if (and (fboundp 'greader-read-asynchronous) (boundp 'greader-synth-process))
      (if-let ((text (buffer-substring-no-properties from to)))
          (progn
            (greader-read-asynchronous text)
            (setq org-ai-talk--reading-process greader-synth-process)
            (org-ai-talk--wait-for-greader callback))
        (warn "no sentence"))
    (error "Greader not installed")))

(defun org-ai-talk--read-region-macos (from to &optional callback)
  "Read the region from `FROM' to `TO'.
If `CALLBACK' is non-nil, call it when done.
For MacOS only, uses the `say' cli utility."
  (let ((buf (current-buffer)))
    (if-let ((text (buffer-substring-no-properties from to)))
        (let* ((voice-param (if org-ai-talk-say-voice (list "-v" org-ai-talk-say-voice) nil))
               (rate-param (if org-ai-talk-say-words-per-minute (list "-r" (number-to-string org-ai-talk-say-words-per-minute)) nil))
               (cmd (append '("say") voice-param rate-param (list text))))
          (setq org-ai-talk--reading-process
                (make-process :name "*org-ai-talker*"
                              :command cmd
                              :sentinel (lambda (&optional _process event)
                                          (when (string= event "finished\n")
                                            (setq org-ai-talk--reading-process nil)
                                            (with-current-buffer buf (goto-char to))
                                            (when callback
                                              (funcall callback)))))))
      (warn "no sentence"))))

(defun org-ai-talk--wait-for-greader (&optional callback)
  "Sort of busy wait for greader to finish.
If `CALLBACK' is non-nil, call it when done."
  (run-with-idle-timer 0.5
                       nil
                       (lambda (callback)
                         (if (and org-ai-talk--reading-process (process-live-p org-ai-talk--reading-process))
                             (org-ai-talk--wait-for-greader callback)
                           (setq org-ai-talk--reading-process nil)
                           (when callback (funcall callback))))
                       callback))

(defun org-ai-talk--read-next-sentence (&optional callback)
  "Read the next sentence.
If `CALLBACK' is non-nil, call it when done."
  (org-ai-talk-read-region (point) (progn (forward-sentence) (point)) callback))

(defun org-ai-talk-stop ()
  "Stop recording and synthesis."
  (interactive)
  (when (and org-ai-talk--reading-process (process-live-p org-ai-talk--reading-process))
    (delete-process org-ai-talk--reading-process))

  (when (org-ai-talk--read-buffer-alive-p)
    (kill-buffer org-ai-talk--read-buffer)
    (setq org-ai-talk--read-buffer nil))

  (when org-ai-talk-use-greader
    (condition-case _
        (when (fboundp 'greader-stop)
          (greader-stop))
      (error nil)))

  (setq org-ai-talk--reading-process nil)
  (setq org-ai-talk--current-insertion-point 0)
  (setq org-ai-talk--is-done t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; the actual useful commands

(defun org-ai-talk-capture-everywhere (&optional output-buffer)
  "The same as `ORG-AI-PROMPT' but uses speech input.
If `OUTPUT-BUFFER' is non-nil, insert the response there."
  (interactive)
  (let ((output-buffer (or (current-buffer) output-buffer)))
    (org-ai-talk--record-and-transcribe-speech (lambda (spoken-text)
                                                 (with-current-buffer output-buffer
                                                   (insert spoken-text)
                                                   (insert "\n\n"))
                                                 (org-ai-prompt spoken-text :output-buffer output-buffer))
                                               "Say something then press any key...")))

(defun org-ai-talk-capture-in-org ()
  "The same as `ORG-AI-PROMPT' but uses speech input."
  (interactive)
  (if-let* ((context (org-ai-special-block))
            (content-end (org-element-property :contents-end context)))
      (progn
        (goto-char content-end)
        (backward-char 1)
        (org-ai-talk--record-and-transcribe-speech
         (lambda (content)
           (insert content)
           ;; need check here to make linter happy
           (when (fboundp 'org-ai-complete-block)
             (org-ai-complete-block)))
         "Say something then press any key..."))
    (org-ai-talk-capture-everywhere)))

(defvar org-ai-after-chat-insertion-hook)

(defun org-ai-talk-output-enable ()
  "Speak text coming from the AI."
  (interactive)
  (add-hook 'org-ai-after-chat-insertion-hook #'org-ai-talk--speak-inserted-text)
  (message "org-ai speech output activated"))

(defun org-ai-talk-output-disable ()
  "Disable speaking text coming from the AI."
  (interactive)
  (remove-hook 'org-ai-after-chat-insertion-hook #'org-ai-talk--speak-inserted-text)
  (message "org-ai speech output deactivated"))

(defun org-ai-talk-output-toggle ()
  "Toggle speaking text coming from the AI."
  (interactive)
  (if (member 'org-ai-talk--speak-inserted-text org-ai-after-chat-insertion-hook)
      (org-ai-talk-output-disable)
    (org-ai-talk-output-enable)))

(defun org-ai-talk-input-toggle ()
  "Toggle speech input for `org-ai-prompt' and `org-ai-talk-on-region' commands."
  (interactive)
  (setq org-ai-talk-spoken-input (not org-ai-talk-spoken-input))
  (message "org-ai speech input %s" (if org-ai-talk-spoken-input "activated" "deactivated")))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-talk)

;;; org-ai-talk.el ends here
