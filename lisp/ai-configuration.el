;;; ai.el --- AI-related configurations -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; This file contains configurations and functions related to AI tools like
;; gptel and aidermacs.

;;; Code:

(defconst my/gptel-title-case-preference
  "Use sentence case for titles unless the context shows a different pattern."
  "General preference for title casing in AI-generated text.")

(defconst my/gptel-base-system-prompt "Return only ASCII. Be succinct."
  "Base system prompt for AI interactions, ensuring ASCII output and conciseness.")

(use-package
 gptel
 :bind
 (("C-c g c" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g m" . gptel-menu)
  ("C-c g x" . gptel-abort))
 :config (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))

 (let ((gemini-key
        (auth-source-pick-first-password
         :host "generativelanguage.googleapis.com")))
   (when gemini-key
     (let ((gemini-backend
            (gptel-make-gemini "Gemini" :key gemini-key :stream t)))
       (setq
        gptel-model 'gemini-2.5-flash-preview-05-20
        gptel-backend gemini-backend)))))

(use-package
 gptel-aibo
 :init (define-prefix-command 'gptel-aibo-map)
 :bind (("C-c g a" . gptel-aibo-map))
 :config
 (define-key gptel-aibo-map (kbd "a") #'gptel-aibo)
 (define-key gptel-aibo-map (kbd "s") #'gptel-aibo-summon)
 (define-key gptel-aibo-map (kbd "c") #'gptel-aibo-complete-at-point))

;; Define custom functions outside use-package so they are available
;; immediately.

(defun my/gptel-strip-markdown-code-block (text)
  "Remove leading/trailing triple backticks and optional language hints from TEXT."
  (let ((stripped text))
    (setq stripped
          (replace-regexp-in-string "\\`\\s-*```[a-zA-Z]*\\s-*\n" "" stripped))
    (setq stripped (replace-regexp-in-string "\\s-*```\\s-*\\'" "" stripped))
    stripped))

(defun my/gptel-process-commit-message-string (response)
  "Process the AI RESPONSE and return the formatted string."
  (let* ((msg (string-trim (my/gptel-strip-markdown-code-block response)))
         (parts (split-string msg "\n\n" t)))
    (string-join parts "\n\n"))) ; Join parts back with double newline

(defconst my/gptel-commit-system-prompt
  (concat
   my/gptel-base-system-prompt
   " You are a concise assistant that writes conventional Git commit messages. Write in imperative tone. Return only the commit message, no formatting, no comments, no explanations, and no repetition of the input. Keep the title under 50 characters. Format the body so no line is longer than 72 characters. If needed, add a body after a blank line. No lists. Separate subtopics into paragraphs. Do not include code blocks. Always refer to functions, commands, files, directory, modules, or package names using backticks, also in the title, for example, `use-package`, `gptel`, or `magit`. Use conventional commits ('feat: add new feature') only if the majority of previous commits shows that pattern. Be consistent with capitalization and backticks between title and body. Do not use abbreviations, eg use 'configuration' instead of 'config'. Add the intention for the change in the body after the change description. Separate the body into sensible paragraphs if applicable.")
  "System prompt used for GPT-based commit message generation and rewriting.")

(defun my/gptel-get-recent-commits ()
  "Get the last Git commit messages with title and body from the current repository."
  (interactive)
  (let*
      ((max-commits 15)
       (max-diff-chars-per-commit 5000)
       (repo-root
        (or (when (fboundp 'magit-toplevel)
              (magit-toplevel))
            (string-trim
             (shell-command-to-string "git rev-parse --show-toplevel"))))
       (default-directory repo-root)
       ;; Always skip the HEAD commit when this function is called. This
       ;; ensures the current commitis not used as history when amending it.
       (log-command
        (format
         "git --no-pager log --skip 1 -n %d --pretty=format:'%%H::%%s' --reverse"
         max-commits))
       (commit-lines
        (split-string (shell-command-to-string log-command) "\n" t))
       (result ""))

    (dolist (line commit-lines)
      (let* ((parts (split-string line "::"))
             (hash (car parts))
             (summary (cadr parts))
             (diff
              (shell-command-to-string
               (format "git --no-pager show --no-color --format=%%b %s" hash)))
             (diff-truncated
              (if (> (length diff) max-diff-chars-per-commit)
                  (substring diff 0 max-diff-chars-per-commit)
                diff))
             (entry
              (format "\n--- Commit: %s (%s) ---\n%s\n"
                      summary
                      hash
                      diff-truncated)))
        (setq result (concat result entry))))

    (if (called-interactively-p 'interactive)
        (with-current-buffer (get-buffer-create "*gptel-commits*")
          (erase-buffer)
          (insert result)
          (pop-to-buffer (current-buffer)))
      (string-trim result))))

(defun my/gptel-generate-commit-message ()
  "Generate a commit message using gptel based on the diff in the current commit buffer."
  (interactive)
  (unless (bound-and-true-p git-commit-mode)
    (user-error "This command must be run in a git-commit buffer"))
  (let* ((diff (buffer-string))
         (recent-commits (my/gptel-get-recent-commits))
         (prompt
          (concat
           "Recent commit messages:\n\n"
           recent-commits
           "\n\nWrite a Git commit message for the following diff:\n\n"
           diff)))
    (require 'gptel)
    (let ((gptel-include-reasoning nil))
      (gptel-request
       prompt
       :system my/gptel-commit-system-prompt
       :callback
       (lambda (response _buffer)
         (when (buffer-live-p (current-buffer))
           (with-current-buffer (current-buffer)
             (save-excursion
               (goto-char (point-min))
               (let* ((processed-message
                       (my/gptel-process-commit-message-string response))
                      (start (point)))
                 (insert processed-message)
                 (let ((end (point)))
                   (fill-region start end)))))))))))

(defun my/gptel-rewrite-commit-message ()
  "Rewrite the current commit message using gptel with a user-defined prompt.
Inserts the rewritten commit message at the top of the buffer, separated by a line."
  (interactive)
  (unless (bound-and-true-p git-commit-mode)
    (user-error "This command must be run in a git-commit buffer"))
  (let* ((buffer-contents (buffer-string))
         (recent-commits (my/gptel-get-recent-commits))
         (user-prompt (read-string "Rewrite instructions: ")))
    (require 'gptel)
    (gptel-request
     (concat
      "Recent commit messages:\n\n"
      recent-commits
      "\n\nUser rewrite instructions:\n\n"
      user-prompt
      "\n\nRewrite the commit message part of the following buffer content based on the instructions and recent commits. Only return the rewritten commit message:\n\n"
      buffer-contents)
     :system my/gptel-commit-system-prompt
     :callback
     (lambda (response _buffer)
       (when (buffer-live-p (current-buffer))
         (with-current-buffer (current-buffer)
           (save-excursion
             (goto-char (point-min))
             (let* ((processed-message
                     (my/gptel-process-commit-message-string response))
                    (start (point))) ; Start of the new message
               (insert processed-message) ; Insert the new message
               (let ((message-end (point))) ; End of the new message
                 (fill-region start message-end) ; Fill the region of the inserted message
                 (goto-char message-end) ; Move to the end of the inserted message
                 (insert "\n\n---\n\n"))))))))))

(eval-after-load "git-commit"
  '(progn
     (when (boundp 'git-commit-mode-map)
       (define-prefix-command 'my/gptel-commit-map)
       (define-key git-commit-mode-map (kbd "C-c g g") 'my/gptel-commit-map)
       (define-key
        my/gptel-commit-map (kbd "c") #'my/gptel-generate-commit-message)
       (define-key
        my/gptel-commit-map (kbd "r") #'my/gptel-rewrite-commit-message))))

(defun my/gptel-generate-branch-name ()
  "Prompt for branch purpose, generate branch name with GPT, then let user edit it.
Then, prompt for the starting point, and finally create and checkout the new branch using Magit."
  (interactive)
  (let*
      ((description (read-string "Describe the purpose of the new branch: "))
       (prompt
        (concat
         "You are a Git expert. Convert the following description into a concise, kebab-case branch name. Use a relevant prefix based on conventional commits like 'feat/', 'fix/', or 'chore/'. Only return the branch name: no quotes, punctuation, or explanations. No abbreviations."
         description)))
    (require 'gptel)
    (let ((gptel-include-reasoning nil))
      (gptel-request
       prompt
       :callback
       (lambda (response _buffer)
         (let* ((branch-name (string-trim response))
                (final-name (read-string "Edit branch name: " branch-name))
                (start-point
                 (magit-read-branch-or-commit
                  "Start point (e.g., 'main', 'HEAD', 'commit-sha'): " nil)))
           (kill-new final-name)
           (message "Final branch name: %s (copied to kill ring)" final-name)
           (when (and (fboundp 'magit-branch-create) (fboundp 'magit-checkout))
             (magit-branch-create final-name start-point)
             (magit-checkout final-name)
             (message "Branch '%s' created from '%s' and checked out."
                      final-name
                      start-point))
           (unless (and (fboundp 'magit-branch-create)
                        (fboundp 'magit-checkout))
             (message
              "Magit functions `magit-branch-create` or `magit-checkout` not found. Branch not created automatically."))))))))

(define-key global-map (kbd "C-c g b") #'my/gptel-generate-branch-name)

(defconst my/gptel-coding-base-system-prompt
  (concat
   my/gptel-base-system-prompt
   " You are a proficient coder. Separate title from body. Only include arguments as continuous text.")
  "System prompt for AI interactions related to coding tasks.")

(defun my/gptel-replace-with-docstring ()
  "Add a minimalist docstring to selected code region using GPTel."
  (interactive)
  (unless (use-region-p)
    (user-error "Please select a region containing the function code"))
  (let*
      ((code (buffer-substring-no-properties (region-beginning) (region-end)))
       (prompt
        (concat
         "Insert a minimalist one-line docstring string in an imperative tone into this logic. "
         "Only return the updated version, without backticks or markdown formatting. "
         "If there is a docstring already, update it based on the new logic."))
       (system my/gptel-coding-base-system-prompt)
       (beg (region-beginning))
       (end (region-end)))
    (require 'gptel)
    (let ((gptel-include-reasoning nil))
      (gptel-request
       (concat prompt "\n\n" code)
       :system system
       :callback
       (lambda (response _buffer)
         (let ((doced-fn
                (string-trim (my/gptel-strip-markdown-code-block response))))
           (when (buffer-live-p (current-buffer))
             (with-current-buffer (current-buffer)
               (save-excursion
                 (delete-region beg end)
                 (goto-char beg)
                 (insert doced-fn))))))))))

(define-key prog-mode-map (kbd "C-c g d") #'my/gptel-replace-with-docstring)

(defun my/gptel-subtle-improvement ()
  "Improve the selected region, correcting obvious mistakes and refining style."
  (interactive)
  (unless (use-region-p)
    (user-error "Please select a region to improve"))
  (let*
      ((beg (region-beginning))
       (end (region-end))
       (code (buffer-substring-no-properties beg end))
       (prompt
        (concat
         "Improve the following content subtly. Make small corrections and stylistic refinements. Do not change the logic. Return only the updated version, no backticks or markdown formatting. Add comments only for parts that are difficult to read. Use spacing and whitespaces as recommended in the respective language style guides."))
       (system my/gptel-coding-base-system-prompt))
    (require 'gptel)
    (let ((gptel-include-reasoning nil))
      (gptel-request
       (concat prompt "\n\n" code)
       :system system
       :callback
       (lambda (response _buffer)
         (let ((new-content (string-trim response)))
           (when (buffer-live-p (current-buffer))
             (save-excursion
               (goto-char beg)
               (delete-region beg end)
               (insert new-content)
               (message "Applied subtle improvements.")))))))))

(define-key prog-mode-map (kbd "C-c g i") #'my/gptel-subtle-improvement)

(defvar my/gptel-word-definition-prompt
  (concat
   my/gptel-base-system-prompt
   " "
   my/gptel-title-case-preference
   " Give a short definition of this word or phrase in a Merriam-Webster style. Provide usage examples, synonyms, and antonyms. Synonyms and antonyms should be comma-separated.")
  "Style prompt used to define a word.")

(defun my/gptel-stash-response (buffer-name prompt response)
  "Store a response in a buffer named BUFFER-NAME and set it to org-mode."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (org-mode)
      (erase-buffer)
      (insert prompt)
      (insert "\n\n---\n\n")
      (insert response))))

(defun my/gptel-define-word (start end)
  "Use an LLM to define the current word of the region."
  (interactive "r")
  (unless (region-active-p)
    (error "No region selected"))
  (let ((input
         (buffer-substring-no-properties (region-beginning) (region-end))))
    (require 'gptel)
    (let ((gptel-include-reasoning nil))
      (gptel-request
       nil
       :callback
       (lambda (response info)
         (my/gptel-stash-response
          (format "*Definition: %s*" input) (plist-get info :context) response)
         (message response))
       :system my/gptel-word-definition-prompt
       :context input))))

(define-key global-map (kbd "C-c g w") #'my/gptel-define-word)

(defvar my/gptel-proof-base-prompt
  (concat
   my/gptel-base-system-prompt
   " "
   my/gptel-title-case-preference
   " Fix spelling, punctuation, and grammer in the following text. Only return the improved version. The returned text should use a line length and breaks as the previous one. Keep whitespace patterns as is.")
  "Base prompt for proof reading.")

(defvar my/gptel-proof-gentle-prompt
  (concat
   my/gptel-proof-base-prompt
   " Where possible, keep the word choice and tone unchanged. Try to keep a Git diff as small as possible."))

(defvar my/gptel-proof-aggressive-prompt
  (concat
   my/gptel-proof-base-prompt
   " Rewrite the text. Be aggressive with improvements."))

(defun my/gptel-proof-apply-fix (buffer marker correction)
  "Apply the suggested changes."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward marker nil t)
      (let* ((end (point))
             (start (- end (length marker))))
        (delete-region start end)
        (insert correction)))))

(defun my/gptel-proof (start end &optional aggressive)
  "Proof-read the selected region between START and END.
If AGGRESSIVE is non-nil (e.g., with C-u prefix), use the aggressive prompt."
  (interactive "r\nP")
  (unless (use-region-p)
    (error "No region selected"))
  (let* ((marker (format "{proof:%s}" (format-time-string "%s%N")))
         (input (buffer-substring start end))
         (prompt-style
          (if aggressive
              "aggressive"
            "gentle"))
         (start-conflict "<<<<<<< Original\n")
         (sep-conflict "=======\n")
         (end-conflict (format ">>>>>>> Proofread (%s)\n" prompt-style)))
    (require 'gptel)
    (save-excursion
      (goto-char start)
      (insert start-conflict)
      (goto-char (+ end (length start-conflict)))
      (insert (concat sep-conflict marker "\n" end-conflict)))
    (let ((gptel-include-reasoning nil))
      (gptel-request
       input
       :callback
       (lambda (response info)
         (if response
             (my/gptel-proof-apply-fix
              (plist-get info :buffer) (plist-get info :context) response)
           (error "Proofread error: %s" (plist-get info :status))))
       :context marker
       :system
       (if aggressive
           my/gptel-proof-aggressive-prompt
         my/gptel-proof-gentle-prompt)))))

(define-key global-map (kbd "C-c g p") #'my/gptel-proof)

(use-package
 aidermacs
 :bind (("C-c a" . aidermacs-transient-menu))
 :config
 (setenv "GEMINI_API_KEY"
         (auth-source-pick-first-password
          :host "generativelanguage.googleapis.com"))
 :custom (aidermacs-default-model "gemini"))

(provide 'ai-configuration)
;;; ai.el ends here
