;;; org-ai-evil-integrations.el --- Evil mode integration for org-ai package -*- lexical-binding: t; -*-

(defgroup org-ai-evil nil
  "Customizable options for org-ai package Evil mode integrations."
  :group 'org-ai)

(defcustom org-ai-evil-cmd-ar "ar"
  "Default Evil Ex command for querying a range in org-ai."
  :type 'string
  :group 'org-ai-evil)

(defcustom org-ai-evil-cmd-cr "cr"
  "Default Evil Ex command for refactoring a range in org-ai."
  :type 'string
  :group 'org-ai-evil)

(when (featurep 'evil)
  ;; Register custom commands with their customizable names
  (evil-ex-define-cmd org-ai-evil-cmd-ar 'evil-org-ai-on-region)
  (evil-define-command evil-org-ai-on-region (start end question)
    "Ask a QUESTION with `org-ai-on-region` for the region from START to END."
    :move-point nil
    (interactive "<r><a>")
    (let ((question (read-string "What do you want to know? ")))
      (org-ai-on-region start end question)))

  (evil-ex-define-cmd org-ai-evil-cmd-cr 'evil-org-ai-change-range)
  (evil-define-command evil-org-ai-change-range (start end question)
    "Ask a QUESTION with `org-ai-refactor-code` for the region from START to END."
    :move-point nil
    (interactive "<r><a>")
    (let ((question (read-string "How do you want to refactor the code? ")))
      (org-ai-refactor-code start end question))))

(provide 'org-ai-evil-integrations)
;;; org-ai-evil-integrations.el ends here
