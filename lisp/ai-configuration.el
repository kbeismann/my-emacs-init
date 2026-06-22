;;; ai-configuration.el --- AI-related configurations -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; This file contains configurations and functions related to AI tools like
;; gptel.

;;; Code:

(require 'json)

(use-package
 gptel
 :bind
 (("C-c g c" . gptel)
  ("C-c g r" . gptel-rewrite)
  ("C-c g m" . gptel-menu)
  ("C-c g x" . gptel-abort))
 :init

 (defconst my/gptel-title-case-preference
   "Use sentence case for titles unless the context shows a different pattern."
   "General preference for title casing in AI-generated text.")

 (defconst my/gptel-base-system-prompt
   "Return only ASCII. Be succinct."
   "Base system prompt for AI interactions, ensuring ASCII output and conciseness.")

 (defun my/gptel-format-error-message (response &optional prefix info)
   "Format an informative error message for GPTel failures.
PREFIX is an optional string to prepend to the error message.
INFO is the plist passed by gptel to the callback, which may contain
additional error details from the API.

Error information is gathered in the following order of precedence:
1. Specific API error/message from the INFO plist.
2. General GPTel error message from `gptel-last-error`.
3. Raw last response from `gptel-last-response` (if the main response was invalid).
4. Indication if the main RESPONSE was nil."
   (let
       ((message
         (or
          prefix
          "GPTel API call failed or returned an empty/invalid response")))
     ;; 1. Prioritize error details from the info plist, as it often contains API-specific errors
     (when info
       (let ((api-error (plist-get info :error))
             (api-message (plist-get info :message)))
         (cond
          (api-error
           (setq message
                 (concat
                  message
                  " (API error: "
                  (prin1-to-string api-error)
                  ")")))
          (api-message
           (setq message
                 (concat
                  message " (API message: " api-message ")"))))))

     ;; 2. Add details from gptel-last-error
     (when (bound-and-true-p gptel-last-error)
       (setq message
             (concat message " (GPTel error: " gptel-last-error ")")))

     ;; 3. Include gptel-last-response if it's an error response and not already covered
     (when
         (and (bound-and-true-p gptel-last-response)
              (not (stringp response))) ;; Only if the main response is not a string (i.e., it's an error)
       (setq message
             (concat
              message
              " (Last raw response: "
              gptel-last-response
              ")")))

     ;; 4. Indicate if the main response was nil
     (when (null response)
       (setq message (concat message ": nil")))
     message))

 (defun my/gptel-strip-markdown-code-block (text)
   "Remove leading/trailing triple backticks and optional language hints from TEXT."
   (let ((stripped text))
     (setq stripped
           (replace-regexp-in-string
            "\\`\\s-*```[a-zA-Z]*\\s-*\n" "" stripped))
     (setq stripped
           (replace-regexp-in-string "\\s-*```\\s-*\\'" "" stripped))
     stripped))


 (defconst my/git-commit-message-project-root
   (expand-file-name "~/.local/share/chezmoi")
   "Project root used by uv for git commit message dependencies.")

 (defconst my/git-commit-message-script
   (expand-file-name "scripts/git_commit_message/main.py"
                     my/git-commit-message-project-root)
   "Path to the shared Python commit-message helper.")

 (defconst my/git-branch-naming-script
   (expand-file-name "scripts/git_branch_naming/main.py"
                     my/git-commit-message-project-root)
   "Path to the shared Python branch-name helper.")

 (defun my/git-commit-message--get-repo-root ()
   "Return the repository root for the current git-commit buffer."
   (let* ((start-directory
           (or (and buffer-file-name
                    (file-name-directory buffer-file-name))
               default-directory))
          (repository-root
           (locate-dominating-file start-directory ".git")))
     (unless repository-root
       (user-error "Could not determine repository root from %s"
                   start-directory))
     (directory-file-name repository-root)))

 (defun my/git-commit-message--run
     (args &optional stdin-content repo-root)
   "Run the shared commit message script with ARGS and optional STDIN-CONTENT.
REPO-ROOT, when non-nil, is used as `default-directory' for the process."
   (unless (file-exists-p my/git-commit-message-script)
     (user-error "Missing commit message script: %s"
                 my/git-commit-message-script))
   (unless (executable-find "uv")
     (user-error "Missing uv executable in PATH"))
   (unless (file-exists-p my/git-commit-message-project-root)
     (user-error "Missing uv project root: %s"
                 my/git-commit-message-project-root))
   (with-temp-buffer
     (let* ((stderr-file
             (make-temp-file "git-commit-message-stderr-" nil ".log"))
            (default-directory (or repo-root default-directory))
            (resolved-args
             (if repo-root
                 (append
                  args
                  (list "--repo-root" (expand-file-name repo-root)))
               args))
            (process-environment
             (let ((clean-environment nil))
               (dolist (environment-entry
                        process-environment
                        (nreverse clean-environment))
                 (unless (string-prefix-p
                          "VIRTUAL_ENV=" environment-entry)
                   (push environment-entry clean-environment)))))
            (command
             (append
              (list
               "uv"
               "run"
               "--project"
               my/git-commit-message-project-root
               "python"
               my/git-commit-message-script)
              resolved-args))
            (output-destination (list t stderr-file))
            (stdout-text nil)
            (stderr-text nil))
       (unwind-protect
           (let ((exit-code
                  (if stdin-content
                      (with-temp-buffer
                        (insert stdin-content)
                        (apply #'call-process-region
                               (point-min)
                               (point-max)
                               (car command)
                               nil
                               output-destination
                               nil
                               (cdr command)))
                    (apply #'call-process
                           (car command)
                           nil
                           output-destination
                           nil
                           (cdr command)))))
             (setq stdout-text (string-trim (buffer-string)))
             (setq stderr-text
                   (if (file-exists-p stderr-file)
                       (string-trim
                        (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string)))
                     ""))
             (cond
              ((not (eq exit-code 0))
               (user-error
                "git-commit-message failed (exit %d). stderr: %s stdout: %s"
                exit-code
                (if (string-empty-p stderr-text)
                    "<empty>"
                  stderr-text)
                (if (string-empty-p stdout-text)
                    "<empty>"
                  stdout-text)))
              ((string-empty-p stdout-text)
               (user-error
                "git-commit-message returned empty output. stderr: %s"
                (if (string-empty-p stderr-text)
                    "<empty>"
                  stderr-text)))
              (t
               stdout-text)))
         (when (file-exists-p stderr-file)
           (delete-file stderr-file))))))

 (defun my/git-branch-naming--run (args &optional repo-root)
   "Run the shared branch naming script with ARGS.
REPO-ROOT, when non-nil, is used as `default-directory' for the process."
   (unless (file-exists-p my/git-branch-naming-script)
     (user-error "Missing branch naming script: %s"
                 my/git-branch-naming-script))
   (unless (executable-find "uv")
     (user-error "Missing uv executable in PATH"))
   (unless (file-exists-p my/git-commit-message-project-root)
     (user-error "Missing uv project root: %s"
                 my/git-commit-message-project-root))
   (with-temp-buffer
     (let* ((stderr-file
             (make-temp-file "git-branch-naming-stderr-" nil ".log"))
            (default-directory (or repo-root default-directory))
            (resolved-args
             (if repo-root
                 (append
                  args
                  (list "--repo-root" (expand-file-name repo-root)))
               args))
            (process-environment
             (let ((clean-environment nil))
               (dolist (environment-entry
                        process-environment
                        (nreverse clean-environment))
                 (unless (string-prefix-p
                          "VIRTUAL_ENV=" environment-entry)
                   (push environment-entry clean-environment)))))
            (command
             (append
              (list
               "uv"
               "run"
               "--project"
               my/git-commit-message-project-root
               "python"
               my/git-branch-naming-script)
              resolved-args))
            (output-destination (list t stderr-file))
            (stdout-text nil)
            (stderr-text nil))
       (unwind-protect
           (let ((exit-code
                  (apply #'call-process
                         (car command)
                         nil
                         output-destination
                         nil
                         (cdr command))))
             (setq stdout-text (string-trim (buffer-string)))
             (setq stderr-text
                   (if (file-exists-p stderr-file)
                       (string-trim
                        (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string)))
                     ""))
             (cond
              ((not (eq exit-code 0))
               (user-error
                "git-branch-naming failed (exit %d). stderr: %s stdout: %s"
                exit-code
                (if (string-empty-p stderr-text)
                    "<empty>"
                  stderr-text)
                (if (string-empty-p stdout-text)
                    "<empty>"
                  stdout-text)))
              ((string-empty-p stdout-text)
               (user-error
                "git-branch-naming returned empty output. stderr: %s"
                (if (string-empty-p stderr-text)
                    "<empty>"
                  stderr-text)))
              (t
               stdout-text)))
         (when (file-exists-p stderr-file)
           (delete-file stderr-file))))))

 (defun my/git-branch-naming--current-commit ()
   "Return the current commit used for branch-name generation."
   (if (and (eq major-mode 'magit-revision-mode)
            (boundp 'magit-buffer-revision)
            (symbol-value 'magit-buffer-revision))
       (symbol-value 'magit-buffer-revision)
     "HEAD"))

 (defun my/git-branch-naming--generate (description commit repo-root)
   "Generate a unique branch name from DESCRIPTION, COMMIT, and REPO-ROOT."
   (let ((args (list "generate" "--commit" commit)))
     (unless (string-empty-p description)
       (setq args (append args (list "--description" description))))
     (my/git-branch-naming--run args repo-root)))

 (defun my/git-branch-naming--unique (branch-name repo-root)
   "Return a unique branch name for BRANCH-NAME in REPO-ROOT."
   (my/git-branch-naming--run
    (list "unique" "--branch-name" branch-name) repo-root))

 (defun my/git-commit-message--collect-current-diff ()
   "Collect diff text from the current commit buffer."
   (save-excursion
     (goto-char (point-min))
     (let ((start-marker
            (or
             (search-forward
              "# ------------------------ >8 ------------------------"
              nil t)
             (search-forward "diff --git " nil t))))
       (if start-marker
           (progn
             (goto-char start-marker)
             (when (looking-at ".*>8.*\n")
               (forward-line 1))
             (buffer-substring-no-properties (point) (point-max)))
         ""))))

 (defun my/git-commit-message--display-report (report-file)
   "Display git commit message REPORT-FILE information transparently."
   (when (and report-file (file-exists-p report-file))
     (let* ((json-object-type 'alist)
            (json-array-type 'list)
            (report (json-read-file report-file))
            (mode (or (cdr (assoc 'mode report)) "unknown"))
            (instruction-sources
             (or (cdr (assoc 'instruction_sources report)) '()))
            (considered-files
             (or (cdr (assoc 'considered_files report)) '()))
            (instruction-text
             (if instruction-sources
                 (string-join instruction-sources ", ")
               "none"))
            (files-text
             (if considered-files
                 (string-join considered-files ", ")
               "none")))
       (message
        "git-commit-message mode=%s; instruction sources=%s; considered files=%s"
        mode instruction-text files-text))))

 (defun my/git-commit-message-generate ()
   "Generate a commit message via the shared Python helper script."
   (interactive)
   (unless (bound-and-true-p git-commit-mode)
     (user-error "This command must be run in a git-commit buffer"))
   (let* ((repo-root (my/git-commit-message--get-repo-root))
          (current-diff (my/git-commit-message--collect-current-diff))
          (diff-file
           (make-temp-file "git-commit-message-diff-" nil ".txt"))
          (report-file
           (make-temp-file "git-commit-message-report-" nil ".json"))
          (generated-message nil))
     (unwind-protect
         (progn
           (when (string-empty-p current-diff)
             (user-error "No diff found in the commit buffer"))
           (write-region current-diff nil diff-file nil 'silent)
           (setq generated-message
                 (my/git-commit-message--run
                  (list
                   "generate"
                   "--diff-file"
                   diff-file
                   "--report-file"
                   report-file)
                  nil repo-root))
           (save-excursion
             (goto-char (point-min))
             (insert generated-message)
             (let ((end (point)))
               (fill-region (point-min) end)
               (goto-char end)
               (insert "\n\n")))
           (my/git-commit-message--display-report report-file))
       (when (file-exists-p diff-file)
         (delete-file diff-file))
       (when (file-exists-p report-file)
         (delete-file report-file)))))

 (defun my/git-commit-message-rewrite ()
   "Rewrite the current commit message via the shared Python helper script."
   (interactive)
   (unless (bound-and-true-p git-commit-mode)
     (user-error "This command must be run in a git-commit buffer"))
   (let* ((repo-root (my/git-commit-message--get-repo-root))
          (buffer-contents (buffer-string))
          (report-file
           (make-temp-file "git-commit-message-report-" nil ".json"))
          (user-prompt (read-string "Rewrite instructions: "))
          (rewritten-message
           (my/git-commit-message--run
            (list
             "rewrite"
             "--instruction"
             user-prompt
             "--report-file"
             report-file)
            buffer-contents repo-root)))
     (save-excursion
       (goto-char (point-min))
       (insert rewritten-message)
       (let ((message-end (point)))
         (fill-region (point-min) message-end)
         (goto-char message-end)
         (insert "\n\n---\n\n")))
     (my/git-commit-message--display-report report-file)
     (when (file-exists-p report-file)
       (delete-file report-file))))

 (eval-after-load "git-commit"
   '(progn
      (when (boundp 'git-commit-mode-map)
        (define-prefix-command 'my/git-commit-message-map)
        (define-key
         git-commit-mode-map
         (kbd "C-c g g")
         'my/git-commit-message-map)
        (define-key
         my/git-commit-message-map
         (kbd "c")
         #'my/git-commit-message-generate)
        (define-key
         my/git-commit-message-map
         (kbd "r")
         #'my/git-commit-message-rewrite))))

 (defun my/git-branch-naming-generate ()
   "Prompt for branch purpose and create a new unique branch with Magit.
Generate a branch name with the shared helper, let the user edit it, prompt for
the starting point, and finally create and checkout the new branch using Magit."
   (interactive)
   (let* ((repo-root
           (or (when (fboundp 'magit-toplevel)
                 (magit-toplevel))
               default-directory))
          (description
           (if (eq major-mode 'magit-revision-mode)
               (buffer-string)
             (read-string
              "Describe the purpose of the new branch: ")))
          (commit (my/git-branch-naming--current-commit))
          (branch-name
           (string-trim
            (my/git-branch-naming--generate
             description commit repo-root)))
          (edited-name
           (string-trim
            (read-string "Edit branch name: " branch-name)))
          (final-name
           (string-trim
            (my/git-branch-naming--unique edited-name repo-root)))
          (start-point
           (magit-read-branch-or-commit
            "Start point (e.g., 'main', 'HEAD', 'commit-sha'): "
            nil)))
     (when (string= final-name "")
       (user-error "Branch name cannot be empty"))
     (kill-new final-name)
     (if (string= final-name edited-name)
         (message "Final branch name: %s (copied to kill ring)"
                  final-name)
       (message (concat
                 "Adjusted branch name to unique value: %s "
                 "(copied to kill ring)")
                final-name))
     (when (and (fboundp 'magit-branch-create)
                (fboundp 'magit-checkout))
       (magit-branch-create final-name start-point)
       (magit-checkout final-name)
       (message "Branch '%s' created from '%s' and checked out."
                final-name
                start-point))
     (unless (and (fboundp 'magit-branch-create)
                  (fboundp 'magit-checkout))
       (message
        (concat
         "Magit functions `magit-branch-create` or `magit-checkout` "
         "not found. Branch not created automatically.")))))

 (define-key
  global-map (kbd "C-c g b") #'my/git-branch-naming-generate)


 (defvar my/gptel-proof-base-prompt
   (concat
    my/gptel-base-system-prompt " " my/gptel-title-case-preference
    (concat
     " Fix spelling, punctuation, and grammar in the following text. "
     "Only return the improved version. "
     "The returned text should use a line length and breaks as the previous one. "
     "Keep whitespace patterns as is."))
   "Base prompt for proof reading.")

 (defvar my/gptel-proof-gentle-prompt
   (concat
    my/gptel-proof-base-prompt
    (concat
     " Where possible, keep the word choice and tone unchanged. "
     "Try to keep a Git diff as small as possible.")))

 (defvar my/gptel-proof-aggressive-prompt
   (concat
    my/gptel-proof-base-prompt
    (concat
     " Rewrite the text. " "Be aggressive with improvements.")))

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
          (end-conflict
           (format ">>>>>>> Proofread (%s)\n" prompt-style)))
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
          (if (stringp response)
              (my/gptel-proof-apply-fix
               (plist-get info :buffer)
               (plist-get info :context)
               response)
            (user-error
             (my/gptel-format-error-message
              response "Proofread error" info))))
        :context marker
        :system
        (if aggressive
            my/gptel-proof-aggressive-prompt
          my/gptel-proof-gentle-prompt)))))

 (define-key global-map (kbd "C-c g p") #'my/gptel-proof)
 :config
 (let ((openrouter-key
        (auth-source-pick-first-password :host "openrouter.ai")))
   (when openrouter-key
     (let ((openrouter-backend
            (gptel-make-openai
             "OpenRouter"
             :host "openrouter.ai"
             :endpoint "/api/v1/chat/completions"
             :stream t
             :key openrouter-key
             :models '(google/gemini-3-flash-preview))))
       (setq
        gptel-backend openrouter-backend
        gptel-model 'google/gemini-3-flash-preview)))))


(provide 'ai-configuration)
;;; ai-configuration.el ends here
