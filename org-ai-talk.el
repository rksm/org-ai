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

(require 'whisper)
(require 'org-ai-useful)

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

(defvar org-ai--whisper-transcription-buffer nil
  "The buffer into which the speech transcription is written.")

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun org-ai-keyboard-quit ()
  "If there is currently a running request, cancel it."
  (interactive)
  (condition-case _
      (when org-ai--current-request-buffer
        (org-ai-interrupt-current-request))
    (error nil)))

(defun org-ai-talk--install-keyboard-quit-advice ()
  "Cancel current request when `keyboard-quit' is called."
  (unless (advice-member-p #'org-ai-keyboard-quit 'keyboard-quit)
    (advice-add 'keyboard-quit :before #'org-ai-keyboard-quit)))

(defun org-ai-talk--uninstall-keyboard-quit-advice ()
  "Remove the advice that cancels current request when `keyboard-quit' is called."
  (advice-remove 'keyboard-quit #'org-ai-keyboard-quit))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun org-ai-talk--speech-input (then-do &optional prompt)
  "Record speech and call `THEN-DO' with the transcription as argument.
If `PROMPT' is non-nil, display it while recording."
  (lexical-let ((then-do then-do))
    (let* ((buffer (or org-ai--whisper-transcription-buffer
                       (get-buffer-create "*org-ai--whisper-transcription-buffer*")))
           (whisper--point-buffer buffer))
      (setq org-ai--whisper-transcription-buffer buffer)
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
                 (kill-buffer org-ai--whisper-transcription-buffer)
                 (setq org-ai--whisper-transcription-buffer nil))
             (lambda ()
               (let ((content (with-current-buffer org-ai--whisper-transcription-buffer
                                (string-trim (buffer-string)))))
                 (funcall then-do content))
               (kill-buffer org-ai--whisper-transcription-buffer)
               (setq org-ai--whisper-transcription-buffer nil)))))))))

(defun org-ai-talk-everywhere (&optional output-buffer)
  "The same as `ORG-AI-PROMPT' but uses speech input.
If `OUTPUT-BUFFER' is non-nil, insert the response there."
  (interactive)
  (lexical-let ((output-buffer (or (current-buffer) output-buffer)))
    (org-ai-talk--speech-input (lambda (spoken-text)
                                    (with-current-buffer output-buffer
                                      (insert spoken-text)
                                      (insert "\n\n"))
                                    (org-ai-prompt spoken-text :output-buffer output-buffer))
                                  "Say something then press any key...")))

(defun org-ai-talk-in-org (&optional output-buffer)
  "The same as `ORG-AI-PROMPT' but uses speech input.
If `OUTPUT-BUFFER' is non-nil, insert the response there."
  (interactive)
  (if-let* ((context (org-ai-special-block))
            (content-end (org-element-property :contents-end context)))
      (progn
        (goto-char content-end)
        (backward-char 1)
        (org-ai-talk--speech-input (lambda (content)
                                        (insert content)
                                        (org-ai-complete-block))
                                      "Say something then press any key..."))
   (org-ai-talk-everywhere)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defcustom org-ai-talk-use-greader
  (not (string-equal system-type "darwin"))
  "Use greader / espeak for speech synthesis?"
  :type 'boolean
  :group 'org-ai-talk)

(defvar org-ai-talk--current-insertion-point 0)
(defvar org-ai-talk--read-buffer nil)
(defvar org-ai-talk--reading-process nil)
(defvar org-ai-talk--is-done nil)

(defun org-ai-talk--speak-inserted-text (type content)
  "Used with `org-ai-after-chat-insertion-hook'.
When installed, will speak the text that we get back from the AI.

`TYPE' is either 'role or 'text or 'end.
`CONTENT' is the text that was inserted."
  (unless org-ai-talk--read-buffer
    (setq org-ai-talk--read-buffer (get-buffer-create "*org-ai-talk*")))

  (cl-case type
    ('role
     (setq org-ai-talk--current-insertion-point 0)
     (setq org-ai-talk--is-done nil)
     (with-current-buffer org-ai-talk--read-buffer
       (erase-buffer)))

    ('text
     (with-current-buffer org-ai-talk--read-buffer
       (save-excursion (goto-char org-ai-talk--current-insertion-point)
                       (insert content)
                       (setq org-ai-talk--current-insertion-point (point)))
       (when (and (string-match-p "\\." content) (not org-ai-talk--reading-process))
         (org-ai-talk--read-next-sentence (lambda ()
                                            (when org-ai-talk--is-done
                                              (with-current-buffer org-ai-talk--read-buffer
                                                (org-ai-talk-read-region (point) (point-max)))))))))

    ('end
     (unless org-ai-talk--reading-process
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
  (cond
   ((or org-ai-talk-use-greader (string-equal system-type "darwin"))
    (org-ai-talk--read-region-greader from to callback))
   (t
    (org-ai-talk--read-region-macos from to callback))))

(defun org-ai-talk--read-region-greader (from to &optional callback)
  "Read the region from `FROM' to `TO'.
If `CALLBACK' is non-nil, call it when done.
Uses greader / espeak, should work on all platforms where espeak is installed."
  (lexical-let ((buf (current-buffer))
                (callback callback)
                (to to))
    (if-let ((text (buffer-substring-no-properties from to)))
        (org-ai-talk-use-greader
         (greader-read-asynchronous text)
         (setq org-ai-talk--reading-process greader-synth-process)
         (org-ai-talk--wait-for-greader callback))
      (warn "no sentence"))))

(defun org-ai-talk--read-region-macos (from to &optional callback)
  "Read the region from `FROM' to `TO'.
If `CALLBACK' is non-nil, call it when done.
For MacOS only, uses the `say' cli utility."
  (lexical-let ((buf (current-buffer))
                (callback callback)
                (to to))
    (if-let ((text (buffer-substring-no-properties from to)))
        (setq org-ai-talk--reading-process
              (make-process :name "*org-ai-talker*"
                            :command (list "say" text)
                            :sentinel (lambda (&optional process event)
                                        (when (string= event "finished\n")
                                          (setq org-ai-talk--reading-process nil)
                                          (with-current-buffer buf (goto-char to))
                                          (when callback
                                            (funcall callback))))))
      (warn "no sentence"))))

(defun org-ai-talk--wait-for-greader (&optional callback)
  "Sort of busy wait for greader to finish.
If `CALLBACK' is non-nil, call it when done."
  (run-with-idle-timer 0.5
                       nil
                       (lambda (callback)
                         (if (and greader-synth-process (process-live-p greader-synth-process))
                             (org-ai-talk--wait-for-greader callback)
                           (setq org-ai-talk--reading-process nil)
                           (when callback (funcall callback))))
                       callback))

(defun org-ai-talk--read-next-sentence (&optional callback)
  "Read the next sentence.
If `CALLBACK' is non-nil, call it when done."
  (org-ai-talk-read-region (point) (save-excursion (forward-sentence) (point)) callback))

(comment
  (progn
   (setq org-ai-talk--current-insertion-point 0)
   (setq org-ai-talk--is-done nil)
   (let ((text "")) (org-ai-talk--speak-inserted-text 'role text 0 (length text)))
   (let ((text "hello world")) (org-ai-talk--speak-inserted-text 'text text 0 (length text)))
   (let ((text " how. are you?")) (org-ai-talk--speak-inserted-text 'text text 0 (length text)))
   (org-ai-talk--speak-inserted-text 'end "" 0 0))

  (org-ai-talk-read-region (region-beginning) (region-end)))

(defun org-ai-talk-stop ()
  "Stop recording and synthesis."
  (interactive)
  (when (and org-ai-talk--reading-process (process-live-p org-ai-talk--reading-process))
    (delete-process org-ai-talk--reading-process))

  (when org-ai-talk-use-greader
    (condition-case _
        (greader-stop)
      (error nil)))

  (setq org-ai-talk--reading-process nil)
  (setq org-ai-talk--current-insertion-point 0)
  (setq org-ai-talk--is-done t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'org-ai-talk)

;;; org-ai-talk.el ends here
