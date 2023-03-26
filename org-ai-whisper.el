;;; org-ai-whisper.el --- Speech-to-text -*- lexical-binding: t; -*-

(require 'whisper)
(require 'org-ai-useful)

(defun org-ai--whisper-stop-recording (then-do)
  ""
  (when (process-live-p whisper--recording-process)
    (interrupt-process whisper--recording-process))
  (run-with-timer
   0.1
   nil
   (lambda (then-do)
     (if (process-live-p whisper--transcribing-process)
         (run-with-timer 0.1
                         nil
                         (lambda (then-do) (org-ai--whisper-stop-recording then-do))
                         then-do)
       (funcall then-do)))
   then-do))

(defvar org-ai--whisper-transcription-buffer nil
  "")


(defun org-ai--whisper-speech-input (then-do &optional prompt)
  ""
  (lexical-let ((then-do then-do))
    (let* ((buffer (or org-ai--whisper-transcription-buffer
                       (get-buffer-create "*org-ai--whisper-transcription-buffer*")))
           (whisper--point-buffer buffer))
      (setq org-ai--whisper-transcription-buffer buffer)
      (whisper--check-install-and-run nil "whisper-start")
      (when prompt (message prompt))
      (while-no-input
        (let ((key (read-key)))
          ()
          (org-ai--whisper-stop-recording
           (lambda ()
             (let ((content (with-current-buffer org-ai--whisper-transcription-buffer
                              (string-trim (buffer-string)))))
               (funcall then-do content))
             (kill-buffer org-ai--whisper-transcription-buffer)
             (setq org-ai--whisper-transcription-buffer nil))))))))

(defun org-ai-speech-prompt (&optional output-buffer)
  "The same as `org-ai-prompt' but uses speech input."
  (interactive)
  (lexical-let ((output-buffer (or (current-buffer) output-buffer)))
    (org-ai--whisper-speech-input (lambda (content)
                                    (org-ai-prompt content output-buffer))
                                  "Say something then press any key...")))

(defun org-ai-ai-block-speech-prompt (&optional output-buffer)
  "The same as `org-ai-prompt' but uses speech input."
  (interactive)
  (if-let* ((context (org-ai-special-block))
            (content-end (org-element-property :contents-end context)))
      (progn
        (goto-char content-end)
        (backward-char 1)
        (org-ai--whisper-speech-input (lambda (content)
                                        (insert content))
                                      "Say something then press any key..."))
   (org-ai-speech-prompt)))

(provide 'org-ai-whisper)

;;; org-ai-whisper.el ends here
