;;; org-configuration.el --- Org-mode related settings -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; This file contains Org-mode configuration settings.

;;; Code:

(defvar my-readings (concat my-gitdir "my-readings/readings.org")
  "My list of readings.")
(defvar my-org-templates (concat my-init "templates.el")
  "My Org templates.")
(defvar my-roam-notes (concat my-gitdir "my-roam-notes/nodes/")
  "My Roam notes.")
(defvar my-notes (concat my-roam-notes "20250603194556-my_personal_notes.org")
  "My notes.")

(add-hook 'org-mode-hook #'my/add-collapse-to-before-save)

(use-package
 org
 :mode "//.org$"
 :bind*
 (("C-c o a" .
   (lambda ()
     (interactive)
     (org-agenda nil "a")))
  ("C-c o t" .
   (lambda ()
     (interactive)
     (org-agenda nil "t")))
  ("C-c o c" . org-capture))
 (:map
  org-mode-map
  (("C-c o c i" . org-clock-in)
   ("C-c o c o" . org-clock-out)
   ("C-c o s n" . org-toogle-narrow-to-subtree)
   ("C-c o i" . org-id-get-create)
   ("C-c C-x C-q" . org-columns-quit)
   ("C-c u l" . #'my/org-roam-unlink-at-point)
   ("C-c o l" . org-lint)
   ("C-M-q" . #'my/org-fill-buffer)))

 :config
 ;; Configure Org directory settings and load work-related notes.
 (setq org-directory my-roam-notes)
 (setq org-default-notes-file my-notes)
 (setq org-todo-file my-notes)
 (setq org-agenda-files (list org-directory my-roam-notes))
 (let ((work-notes (expand-file-name "notes.el" user-emacs-directory)))
   (if (file-exists-p work-notes)
       (let ()
         (message "%s" "Found work-related notes...")
         (load work-notes))
     (message "%s" "No work-related notes found.")))

 ;; Use relative paths.
 (setq org-link-file-path-type 'relative)

 ;; Startup options.
 (setq org-startup-indented nil)
 (setq org-startup-with-latex-preview nil)
 (setq org-startup-align-all-tables t)

 ;; Indentation.
 (setq org-indent-mode-turns-on-hiding-stars nil)
 (setq org-adapt-indentation nil)

 ;; Miscellaneous.
 (setq org-src-window-setup 'other-window)
 (setq org-tags-column 0)
 (setq org-image-actual-width nil)
 (setq org-highlight-latex-and-related '(latex script entities))
 (setq org-catch-invisible-edits t)

 ;; All child tasks have to be "DONE" before the parent is "DONE."
 (setq org-enforce-todo-dependencies t)

 ;; To-do settings.
 (setq org-hierarchical-todo-statistics nil)

 ;; Logging.
 (setq org-log-done-with-time t)
 (setq org-log-done 'time)
 (setq org-log-repeat 'time)

 ;; Agenda settings.
 (setq org-agenda-skip-scheduled-if-done nil)
 (setq org-agenda-skip-deadline-if-done nil)
 (setq org-agenda-include-deadlines t)
 (setq org-agenda-include-diary nil)
 (setq org-agenda-compact-blocks t)
 (setq org-agenda-start-with-log-mode nil)
 ;; Better calendar settings: Include last week only if today is Monday,
 ;; always show three weeks. and always start the week on Monday.
 (setq calendar-week-start-day 1)
 (setq org-agenda-start-on-weekday 1)
 (setq org-agenda-span 9)

 ;; Style-related.
 (font-lock-add-keywords
  'org-mode
  '(("^ *\\([-]\\) " (0 (prog1 ()
          (compose-region (match-beginning 1) (match-end 1) "•"))))))

 (setq org-agenda-custom-commands
       '(("u" "Unscheduled TODOs" alltodo ""
          ((org-super-agenda-groups
            '((:name
               "Unscheduled"
               :and
               (:todo t :not (:scheduled t) :not (:deadline t)))))))))
 (defun my/org-agenda-unscheduled ()
   "Display org-agenda for custom command 'u' (Unscheduled TODOs)."
   (interactive)
   (org-agenda nil "u"))
 (global-set-key (kbd "C-c o u") 'my/org-agenda-unscheduled)

 (defun my/org-syntax-convert-keyword-case-to-lower ()
   "Convert all #+KEYWORDS to #+keywords."
   (interactive)
   (save-excursion
     (goto-char (point-min))
     (let ((count 0)
           (case-fold-search nil))
       (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
         (unless (s-matches-p "RESULTS" (match-string 0))
           (replace-match (downcase (match-string 0)) t)
           (setq count (1+ count))))
       (message "Replaced %d occurances" count))))

 (defun my/modi/lower-case-org-keywords ()
   "Lower case Org keywords and block identifiers.

             Example: \"#+TITLE\" -> \"#+title\"
                      \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"

             Inspiration: https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
   (interactive)
   (save-excursion
     (goto-char (point-min))
     (let ((case-fold-search nil)
           (count 0))
       (while (re-search-forward
               "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)"
               nil
               :noerror)
         (setq count (1+ count))
         (replace-match (downcase (match-string-no-properties 1))
                        :fixedcase
                        nil
                        nil
                        1))
       (message "Lower-cased %d matches" count))))

 (defun my/align-tags-in-all-org-files (directory)
   "Align tags in all Org files in the specified DIRECTORY."
   (interactive "DSelect directory: ")
   (let ((files (directory-files-recursively directory "\\.org$")))
     (if (not files)
         (message "No Org files found in %s" directory)
       (dolist (file files)
         (message "Processing file: %s" file)
         (let ((buf (find-file-noselect file)))
           (with-current-buffer buf
             (when (derived-mode-p 'org-mode)
               (org-align-tags t)
               (save-buffer)))
           ;; Only kill the buffer if it wasn't already open
           (unless (get-file-buffer file)
             (kill-buffer buf)))
         (message "Aligned tags in %s" file)))))

 (defun my/sort-org-tags-region (beg end &optional reversed)
   "In active region sort tags alphabetically in descending order.
With prefix argument REVERSE order."
   (interactive "r\nP")
   (unless (region-active-p)
     (user-error "No active region to sort!"))
   (let* ((str (s-trim (buffer-substring-no-properties beg end)))
          (wrd (split-string str ":" t " "))
          (new
           (concat
            ":"
            (s-join
             ":"
             (sort wrd
                   (if reversed
                       #'string> ; Sort descending if reversed
                     #'string<))) ; Sort ascending by default
            ":")))
     (save-excursion
       (goto-char beg)
       (delete-region beg end)
       (insert new))))

 (defun my/sort-org-tags-in-buffer ()
   "Sort Org tags in all headlines in the current buffer."
   (interactive)
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward org-tag-line-re nil t)
       (let* ((tags (org-get-tags-string))
              (beg (match-beginning 0))
              (end (match-end 0)))
         (when tags
           (let*
               ((tag-str (string-trim tags ":"))
                (sorted (sort (split-string tag-str ":" t " ") #'string<)) ; Sort ascending
                (new-tag-str (concat ":" (string-join sorted ":") ":")))
             (org-set-tags new-tag-str)))))))

 (defun my/sort-org-tags-in-directory (dir)
   "Sort tags in all Org files under DIR using `my/sort-org-tags-in-buffer`."
   (interactive "DDirectory: ")
   (let ((org-files (directory-files-recursively dir "\\.org$")))
     (dolist (file org-files)
       (message "Processing %s" file)
       (with-current-buffer (find-file-noselect file)
         (when (derived-mode-p 'org-mode)
           (my/sort-org-tags-in-buffer)
           (save-buffer))
         (kill-buffer)))))

 (define-minor-mode my/org-auto-sort-tags-mode
   "Minor mode to auto-sort Org tags on save."
   :lighter
   " Tagsort"
   (if my/org-auto-sort-tags-mode
       (add-hook 'before-save-hook #'my/sort-org-tags-in-buffer nil t)
     (remove-hook 'before-save-hook #'my/sort-org-tags-in-buffer t)))

 (defun my/org-roam-unlink-at-point ()
   "Replace Org-roam link at point with its description, preserving spacing."
   (interactive)
   (let ((element (org-element-context)))
     (when (eq (org-element-type element) 'link)
       (let* ((desc-begin (org-element-property :contents-begin element))
              (desc-end (org-element-property :contents-end element))
              (desc
               (and desc-begin
                    desc-end
                    (buffer-substring-no-properties desc-begin desc-end)))
              (begin (org-element-property :begin element))
              (end (org-element-property :end element))
              (before
               (save-excursion
                 (goto-char begin)
                 (if (bobp)
                     nil
                   (char-before))))
              (after
               (save-excursion
                 (goto-char end)
                 (if (eobp)
                     nil
                   (char-after)))))
         (when desc
           (delete-region begin end)
           ;; Insert space before if needed
           (when (and before (not (member (char-syntax before) '(?\  ?\( ?\"))))
             (insert " "))
           (insert desc)
           ;; Insert space after if needed
           (when (and after
                      (not
                       (member (char-syntax after) '(?\  ?\) ?\" ?. ?, ?! ??))))
             (insert " ")))))))

 ;; Always insert blank line before headings.
 (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

 ;; Configure Org refiling.
 (setq org-refile-use-outline-path 'full-file-path)
 (setq org-outline-path-complete-in-steps nil)
 (setq org-refile-allow-creating-parent-nodes 'confirm)
 (setq org-refile-targets
       '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))

 (setq org-todo-keywords
       '((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")))
 (setq org-todo-keyword-faces
       '(("TODO" . org-warning)
         ("DONE" . org-done)
         ("CANCELLED" . org-done)
         ("INPROGRESS" . org-link)))

 (let ((templates (expand-file-name "templates.el" user-emacs-directory)))
   (if (and (file-exists-p templates) (boundp 'org-capture-templates))
       (let ()
         (message "%s" "Adding templates for work...")
         (load templates))
     (message "%s" "No work-related templates specified.")
     (setq
      org-capture-templates
      '(("p" "personal")
        ("pt"
         "TODO"
         entry
         (file+headline org-todo-file "To-dos")
         "* TODO \[\#A\] %^{Title} %^g\n\n%i\n\n"
         :empty-lines 1)
        ("pn"
         "Save note"
         entry
         (file+headline org-default-notes-file "Uncategorized")
         "* UNCATEGORIZED [\#A] %^{Title} %^g\n\n\n%i\n\n"
         :empty-lines 1)
        ("pu"
         "Store URL"
         entry
         (file+headline org-default-notes-file "Uncategorized")
         "* UNCATEGORIZED [\#A] %^{Title} %^g\n:PROPERTIES:\n:URL: %x\n:END:\n\n%i\n\n"
         :empty-lines 1)
        ("pr"
         "Save reading"
         entry
         (file buffer-file-name)
         "* TODO [\#C] \"%^{Title}\" %^g:reading:\n:PROPERTIES:\n:URL:\n:Author:\n:Year:\n:END:\n\n%i\n\n"
         :empty-lines 1)
        ("pe"
         "Edit/fix file"
         entry
         (file+headline org-todo-file "To-dos")
         "* TODO [\#C] %^{Title} %^g:code:\n:PROPERTIES:\n:LINK: %a\n:END:\n\n%i\n\n"
         :empty-lines 1)
        ("pu"
         "Save URL and check later"
         entry
         (file+headline org-todo-file "To-dos")
         "* TODO [\#A] %^{Title} %^g:url:\n:PROPERTIES:\n:URL: %x\n:END:\n\n%i\n\n"
         :empty-lines 1)
        ("pm"
         "Meeting minutes"
         entry
         (file+headline org-default-notes-file "Uncategorized")
         "* TODO [\#A] %^{Title} :meeting:minutes:%^g\nSCHEDULED: %T\n:PROPERTIES:\n:END:\n\n- *Attendees:*\n\n  + [X] Karsten Beismann\n\n- *Agenda:*\n\n  1. ...%i\n\n - *Notes:*\n\n  + ...\n\n- *Next steps:*\n\n  + ...\n\n"
         :empty-lines 1)
        ("ps"
         "Stand-up"
         entry
         (file+headline org-default-notes-file "Uncategorized")
         "* TODO [\#A] Stand-up :meeting:standup:%^g\nSCHEDULED: %T\n:PROPERTIES:\n:END:\n\n- *Progress since the last meeting:*\n\n  1. ...%i\n\n- *Outlook:*\n\n  1. ...\n\n - *Questions/collaboration:*\n\n  + ...\n\n- *Notes:*\n\n  + ...\n\n"
         :empty-lines 1)))))

 ;; Switch entry to DONE when all subentries are done, to TODO
 ;; otherwise.
 (defun my/org-summary-todo (n-done n-not-done)
   "Switch entry to DONE when all subentries are done, to TODO otherwise."
   (let (org-log-done-with-time
         org-log-states)
     (org-todo
      (if (= n-not-done 0)
          "DONE"
        "TODO"))))

 ;; Don't confirm before evaluating.
 (setq org-confirm-babel-evaluate nil)
 ;; Available languages: https://orgmode.org/org.html#Languages
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((shell . t) (emacs-lisp . t) (org . t) (python . t) (R . t) (latex . t)))
 (setq org-babel-python-command "python3")
 ;; Better source block behavior.
 (setq
  org-src-preserve-indentation t
  org-edit-src-content-indentation 0)
 ;; Highlight code in code blocks in native language, also use TAB as
 ;; in native language.
 (setq
  org-src-fontify-natively t
  org-src-tab-acts-natively t)
 ;; Change font size for LaTeX previews.
 (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
 (setq org-format-latex-options
       (plist-put org-format-latex-options :html-scale 1.5))
 (setq org-latex-toc-command "\\tableofcontents \\clearpage")

 (defun my/org-get-line-category-at-point-optimized ()
   "Return the category of the current line as a symbol:
   :heading, :metadata, :body, :blank.
   Optimized to reduce calls to `org-element-context`."
   (save-excursion
     (beginning-of-line)
     (let ((line-text (buffer-substring (point) (line-end-position))))
       (cond
        ((string-empty-p line-text)
         :blank)
        ((org-at-heading-p)
         :heading)
        ;; Explicitly check for common metadata lines using regex
        ((or
          (string-match-p "^[ \t]*:PROPERTIES:" line-text) ; Start of property drawer
          (string-match-p "^[ \t]*:END:" line-text) ; End of property drawer
          (string-match-p "^[ \t]*:[A-Z_]+:" line-text) ; Individual property line like :ID:
          (string-match-p
           "^[ \t]*\\(?:CLOSED\\|SCHEDULED\\|DEADLINE\\):"
           line-text) ; Timestamps with keywords
          ;; Org keywords like #+TITLE: (requires colon)
          (let ((match-pos
                 (string-match "^[ \t]*#\\+\\([A-Z_]+\\):" line-text)))
            (and match-pos)))
         :metadata)
        ;; Explicitly check for common body elements with regex before org-element-context
        ((or
          (string-match-p "^[ \t]*#\\+\\(?:begin\\|end\\)_[A-Z_]+" line-text) ; Org block delimiters
          (string-match-p "^[ \t]*[-+*] " line-text) ; List items
          (string-match-p "^[ \t]*[0-9]+\\. " line-text) ; Numbered list items
          (string-match-p "^[ \t]*\\(?:#\\+CALL\\|#\\+RESULTS\\):" line-text) ; Babel call/results
          )
         :body)
        (t
         ;; Fallback to org-element-context for robustness, but it should be less frequent now
         (let ((element (org-element-context)))
           (pcase (org-element-type element)
             ('src-block :body) ; Source blocks
             ('example-block :body) ; Example blocks
             ('quote-block :body) ; Quote blocks
             ('comment-block :body) ; Comment blocks
             ('list-item :body) ; List items
             ('paragraph :body) ; Regular paragraphs
             ;; Default to body for anything else not explicitly categorized
             (_ :body))))))))

 (defun my/org-determine-required-blanks
     (prev-cat
      current-cat
      &optional
      is-first-content-line
      prev-line-text
      current-line-text)
   "Determine the number of blank lines required between two categories.
   Returns 0, 1, or :preserve.
   IS-FIRST-CONTENT-LINE should be non-nil if current-cat is the first content line in the buffer.
   PREV-LINE-TEXT is the text of the previous line, used for special cases.
   CURRENT-LINE-TEXT is the text of the current line, used for special cases."
   (cond
    ;; Special case: no blank before #+TBLFM: regardless of previous category
    ((let ((case-fold-search t))
       (string-match-p "^[ \t]*#\\+tblfm:" current-line-text))
     0)
    (is-first-content-line
     0) ; No blanks before the very first content line
    (t
     (cond
      ((and (eq prev-cat :body)
            (eq current-cat :body)
            (string-match-p "^[ \t]*#\\+begin_[A-Z_]+" prev-line-text))
       0) ; No blank after any block start (e.g., #+begin_src)
      ((and (eq prev-cat :body)
            (eq current-cat :body)
            (string-match-p "^[ \t]*#\\+end_[A-Z_]+" current-line-text))
       0) ; No blank before any block end (e.g., #+end_src)
      ((and (eq prev-cat :heading) (eq current-cat :heading))
       0)
      ((and (eq prev-cat :heading) (eq current-cat :metadata))
       0)
      ((and (eq prev-cat :metadata) (eq current-cat :metadata))
       0)
      ;; Special case: no blank after #+RESULTS:
      ((and (eq prev-cat :metadata)
            (eq current-cat :body)
            (let ((case-fold-search t))
              (string-match-p "^[ \t]*#\\+results:" prev-line-text)))
       0)
      ;; Special case: no blank after #+TBLFM:
      ((and (eq prev-cat :metadata)
            (eq current-cat :body)
            (let ((case-fold-search t))
              (string-match-p "^[ \t]*#\\+tblfm:" prev-line-text)))
       0)
      ;; Special case: no blank before source block if previous is #+NAME:
      ((and (eq prev-cat :metadata)
            (eq current-cat :body)
            (let ((case-fold-search t))
              (and (string-match-p "^[ \t]*#\\+name:" prev-line-text)
                   (string-match-p "^[ \t]*#\\+begin_src" current-line-text))))
       0)
      ((or (and (eq prev-cat :heading) (eq current-cat :body))
           (and (eq prev-cat :metadata) (eq current-cat :heading))
           (and (eq prev-cat :metadata) (eq current-cat :body))
           (and (eq prev-cat :body) (eq current-cat :heading))
           (and (eq prev-cat :body) (eq current-cat :metadata)))
       1)
      ;; Preserve existing blank lines for other combinations (body-body)
      (t
       :preserve)))))

 (defun my/org-normalize-header-spacing ()
   "Ensure consistent empty line handling around Org mode headers and metadata.
    - No empty line between two consecutive headers.
    - No empty line between a header and its associated metadata (e.g., :PROPERTIES:).
    - Exactly one empty line between a header and body text.
    - Exactly one empty line between metadata and a header or body text.
    - No empty line between consecutive metadata lines (e.g., #+TITLE then #+DATE).
    - Preserves existing blank lines between non-header, non-metadata content (body-body).
    - No blank lines at the beginning or end of the buffer.

   This function builds the normalized content in memory and replaces the
   buffer content in a single operation to improve performance and reduce visual
   disruption during saving."
   (interactive)
   (save-window-excursion ; Use save-window-excursion to preserve view
     (let
         ((original-line (line-number-at-pos))
          (original-column (current-column))
          (old-window-start (window-start)) ; Explicitly save window start
          (scroll-conservatively most-positive-fixnum) ; Temporarily disable aggressive scrolling
          (inhibit-redisplay t)) ; Temporarily inhibit redisplay
       (unwind-protect
           (save-restriction
             (widen)
             (let ((new-content-lines '())
                   (prev-line-category :blank)
                   (consecutive-blanks 0)
                   (prev-line-text nil))

               (goto-char (point-min))

               (while (not (eobp))
                 (let* ((current-line-start (point))
                        (current-line-end (line-end-position))
                        (line-text
                         (buffer-substring current-line-start current-line-end))
                        (current-line-category
                         (my/org-get-line-category-at-point-optimized)))

                   (cond
                    ((eq current-line-category :blank)
                     (setq consecutive-blanks (1+ consecutive-blanks)))
                    (t
                     ;; This is a content line
                     (let* ((is-first-content-line (null new-content-lines))
                            (required-blanks
                             (my/org-determine-required-blanks
                              prev-line-category
                              current-line-category
                              is-first-content-line
                              prev-line-text
                              line-text)))

                       ;; Add required blank lines
                       (cond
                        ((eq required-blanks :preserve)
                         ;; Preserve existing blanks for body-body transitions
                         (dotimes (_ consecutive-blanks)
                           (push "" new-content-lines)))
                        ((> required-blanks 0)
                         ;; Add the exact number of required blanks
                         (dotimes (_ required-blanks)
                           (push "" new-content-lines)))))

                     ;; Add the current content line
                     (push line-text new-content-lines)

                     ;; Reset blank counter and update previous category
                     (setq consecutive-blanks 0)
                     (setq prev-line-category current-line-category)
                     (setq prev-line-text line-text))))

                 (forward-line 1))

               ;; Reverse the list to get correct order and join
               (let ((final-content
                      (string-join (nreverse new-content-lines) "\n")))
                 ;; Replace buffer content
                 (delete-region (point-min) (point-max))
                 (insert final-content)
                 ;; Ensure the buffer ends with a newline (standard for files)
                 (goto-char (point-max))
                 (when (not
                        (and (> (point) (point-min)) (eq (char-before) ?\n)))
                   (insert "\n"))))))
       ;; Ensure redisplay is re-enabled and point/window are restored
       (setq inhibit-redisplay nil)
       (goto-line original-line)
       (move-to-column original-column)
       (set-window-start (selected-window) old-window-start)
       (redisplay))))

 (defun my/org-normalize-header-spacing-in-directory (directory)
   "Apply `my/org-normalize-header-spacing` to all Org files in DIRECTORY.
   Processes files recursively in subdirectories."
   (interactive "DSelect directory: ")
   (let ((org-files (directory-files-recursively directory "\\.org$")))
     (if (not org-files)
         (message "No Org files found in %s" directory)
       (dolist (file org-files)
         (message "Processing file: %s" file)
         (with-current-buffer (find-file-noselect file)
           (when (derived-mode-p 'org-mode)
             (my/org-normalize-header-spacing)
             (save-buffer))
           (message "Normalized header spacing in %s" file))))))

 (defun my/org-fill-buffer ()
   "Fill all paragraphs in the current Org mode buffer, ignoring source blocks and logbook entries.
This function iterates through the buffer, applying `org-fill-paragraph`
to each paragraph to ensure consistent line wrapping, but skips over
any Org source blocks and logbook entries."
   (interactive)
   (save-excursion
     (widen) ; Ensure all parts of the buffer are visible
     (goto-char (point-min))
     (while (not (eobp))
       (let ((element (org-element-context))
             (line-start (point))
             (line-end (line-end-position)))
         (cond
          ((eq (org-element-type element) 'src-block)
           ;; If inside a source block, skip to its end
           (goto-char (org-element-property :end element)))
          ;; NEW: If it's a logbook entry, skip to the next line
          ((string-match-p
            "^[ \t]*[-+*] \\(?:State\\|Note\\) \"[^\"]+\" \\(?:from \"[^\"]+\" \\)?\\[[0-9-]+\\s-+[A-Za-z]+\\s-+[0-9:]+\\]"
            (buffer-substring-no-properties line-start line-end))
           (forward-line 1))
          (t
           ;; Otherwise, fill the current paragraph and move to the next
           (org-fill-paragraph)
           (forward-paragraph)))))))

 (defvar my/org-shfmt-verbose t
   "If non-nil, show a summary: how many blocks formatted/warned.")

 (defun my/org-format-sh-blocks ()
   "Format all sh/bash/shell source block bodies with shfmt.
Only modifies the text between #+begin_src and #+end_src (narrowed).
On success: replace with shfmt output (preserving list indentation).
On failure: keep body and insert/update a one-line warning at the top."
   (interactive)
   (when (and (derived-mode-p 'org-mode) (executable-find "shfmt"))
     (save-excursion
       (save-restriction
         (widen)
         (let ((formatted 0)
               (warned 0)
               regions)
           ;; collect body regions
           (org-element-map
            (org-element-parse-buffer) 'src-block
            (lambda (blk)
              (let ((lang
                     (downcase (or (org-element-property :language blk) ""))))
                (when (member lang '("sh" "bash" "shell"))
                  (save-excursion
                    (goto-char blk-beg)
                    (when (re-search-forward "^[ \t]*#\\+begin_src\\b.*$"
                                             blk-end
                                             t)
                      (forward-line)
                      (let ((body-beg (point)))
                        (when (re-search-forward "^[ \t]*#\\+end_src\\b"
                                                 blk-end
                                                 t)
                          (let ((body-end (match-beginning 0)))
                            (when (< body-beg body-end)
                              (push (cons body-beg body-end) regions))))))))))
            nil nil)

           ;; process bottom-up
           (dolist (region (sort regions (lambda (a b) (> (car a) (car b)))))
             (condition-case err
                 (let ((rb (car region))
                       (re (cdr region)))
                   (save-restriction
                     (narrow-to-region rb re)
                     (let* ((orig
                             (buffer-substring-no-properties
                              (point-min) (point-max)))
                            (orig-ends-nl (string-suffix-p "\n" orig))
                            (lines (split-string orig "\n" nil))
                            ;; common indent width across non-empty lines
                            (min-indent
                             (let (m)
                               (dolist (ln lines m)
                                 (unless (string-match-p "\\`[ \t]*\\'" ln)
                                   (string-match "\\`[ \t]*" ln)
                                   (let ((w (length (match-string 0 ln))))
                                     (setq m
                                           (if (null m)
                                               w
                                             (min m w))))))))
                            (indent-width (or min-indent 0))
                            (indent-prefix (make-string indent-width ?\s))
                            ;; deindent safely (regex, no substrings)
                            (rx-deindent
                             (format "\\`[ \t]\\{0,%d\\}" indent-width))
                            (deindented
                             (mapconcat (lambda (ln)
                                          (replace-regexp-in-string
                                           rx-deindent "" ln
                                           nil t))
                                        lines
                                        "\n"))
                            (input
                             (if (string-suffix-p "\n" deindented)
                                 deindented
                               (concat deindented "\n")))
                            (out (generate-new-buffer " *org-shfmt-out*"))
                            status)
                       (unwind-protect
                           (progn
                             ;; run shfmt *in the temp buffer*, replacing its contents
                             (with-current-buffer out
                               (erase-buffer)
                               (insert input)
                               (setq status
                                     (call-process-region
                                      (point-min)
                                      (point-max)
                                      "shfmt" ; program
                                      t
                                      t
                                      nil))) ; replace, output here, no args
                             (if (and (integerp status) (zerop status))
                                 ;; success: pull result from temp buffer
                                 (let*
                                     ((fmt
                                       (with-current-buffer out
                                         (buffer-string)))
                                      ;; restore newline state like the deindented input
                                      (fmt*
                                       (if (and (string-suffix-p "\n" fmt)
                                                (not
                                                 (string-suffix-p
                                                  "\n" deindented)))
                                           (string-trim-right fmt)
                                         fmt))
                                      ;; reindent non-empty lines
                                      (reindented
                                       (if (zerop indent-width)
                                           fmt*
                                         (mapconcat (lambda (ln)
                                                      (if (string-match-p
                                                           "\\`[ \t]*\\'" ln)
                                                          ln
                                                        (concat
                                                         indent-prefix ln)))
                                                    (split-string fmt* "\n" nil)
                                                    "\n"))))
                                   (delete-region (point-min) (point-max))
                                   (insert reindented)
                                   (cl-incf formatted))
                               ;; failure: add/update a single warning line
                               (let*
                                   ((warning
                                     "# [org-shfmt] format error: non-zero exit")
                                    (body-lines
                                     (split-string deindented "\n" nil)))
                                 (if (and body-lines
                                          (string-match-p
                                           "\\`# \\[org-shfmt\\] format error:"
                                           (car body-lines)))
                                     (setf (car body-lines) warning)
                                   (setq body-lines (cons warning body-lines)))
                                 (let* ((warned-text
                                         (mapconcat #'identity body-lines "\n"))
                                        (warned-text*
                                         (if (string-suffix-p "\n" deindented)
                                             warned-text
                                           (string-trim-right warned-text)))
                                        (reindented
                                         (if (zerop indent-width)
                                             warned-text*
                                           (mapconcat (lambda (ln)
                                                        (if (string-match-p
                                                             "\\`[ \t]*\\'" ln)
                                                            ln
                                                          (concat
                                                           indent-prefix ln)))
                                                      (split-string warned-text*
                                                                    "\n"
                                                                    nil)
                                                      "\n"))))
                                   (delete-region (point-min) (point-max))
                                   (insert reindented)
                                   (cl-incf warned))))
                             (when (buffer-live-p out)
                               (kill-buffer out))))))
                   (error
                    (message "[org-shfmt] internal error on a block: %s"
                             (error-message-string err)))))
             (when my/org-shfmt-verbose
               (message "[org-shfmt] formatted %d block(s), warned %d block(s)."
                        formatted
                        warned)))))))))

(defun my/org-lint-directory (directory)
  "Run org-lint on all Org files in DIRECTORY and its subdirectories.
Stops at the first file with issues, opens it, and runs org-lint interactively."
  (interactive "DSelect directory: ")
  (let ((original-threshold gc-cons-threshold))
    (setq gc-cons-threshold (* 500 1024 1024))
    (let ((org-files (directory-files-recursively directory "\\.org$"))
          (total-files 0)
          (stop nil))
      (if (not org-files)
          (message "No Org files found in %s" directory)
        (dolist (file org-files)
          (when (not stop)
            (cl-incf total-files)
            (message "Linting %s..." (file-relative-name file directory))
            (let ((buf (find-file-noselect file)))
              (with-current-buffer buf
                (when (derived-mode-p 'org-mode)
                  (let ((lint-output (org-lint)))
                    (if lint-output
                        (progn
                          (switch-to-buffer buf)
                          (org-lint)
                          (setq stop t))
                      (kill-buffer buf)))))
              (garbage-collect)))))
      (garbage-collect)
      (setq gc-cons-threshold original-threshold))))

(use-package
 org-super-agenda
 :defer nil
 :after org
 :config
 (setq org-agenda-include-deadlines t)
 (setq org-agenda-block-separator 61)
 (setq org-agenda-compact-blocks nil)
 (org-super-agenda-mode t)

 (let ((work-agenda (expand-file-name "agenda.el" user-emacs-directory)))
   (message "Work agenda: %s" work-agenda)
   (if (file-exists-p work-agenda)
       (prog1 "Load work-related agenda settings."
         (message "%s" "Found work-related agenda settings...")
         (load work-agenda))
     (prog1 "Load private agenda settings."
       (message "%s" "No work-related agenda settings found.")
       (setq org-super-agenda-groups
             '((:name "INPROGRESS" :todo "INPROGRESS" :order -10)
               (:name "Finances" :tag "finances" :order 2)
               (:name
                "Personal"
                :and
                (:tag
                 "personal"
                 :not
                 (:tag
                  ("finances"
                   "shoppinglist"
                   "reading"
                   "@work"
                   "series"
                   "movie")))
                :order 3)
               (:name "Readings" :tag "reading" :order 4)
               (:name "Medical" :tag "medical" :order 5)
               (:name "Shopping list" :tag "shoppinglist" :order 6)
               (:name "Movies" :and (:tag "movie") :order 7)
               (:name "Series" :and (:tag "series") :order 8)
               (:name "Travel" :and (:tag "travel") :order 9)
               (:name "Other" :order 20)))))))

(use-package
 org-appear
 :after org
 :hook (org-mode . org-appear-mode)
 :config (setq org-hide-emphasis-markers t))

(use-package
 org-download
 :disabled t
 :after org
 :bind
 (:map
  org-mode-map
  (("C-c i s" . org-download-screenshot) ("C-c i y" . org-download-yank))))

(use-package
 org-roam
 :bind*
 (("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n t c" . org-roam-dailies-capture-today)
  ("C-c n t g" . org-roam-dailies-goto-today)
  ("C-c n g" . org-roam-graph)
  ("C-c n a r" . org-roam-ref-add)
  ("C-c n a t" . org-roam-tag-add)
  ("C-c n s" . org-roam-db-sync))
 :config
 (setq org-roam-directory my-roam-notes)
 (setq org-roam-db-gc-threshold most-positive-fixnum)
 (setq org-roam-completion-everywhere t)
 (setq
  org-roam-capture-templates
  '(("d"
     "default"
     plain
     "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
     :unnarrowed t)))
 (setq org-roam-dailies-capture-templates
       '(("d"
          "default"
          entry
          "* %?"
          :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
 (setq org-roam-mode-section-functions
       (list
        #'org-roam-backlinks-section
        #'org-roam-reflinks-section
        #'org-roam-unlinked-references-section))
 (org-roam-db-autosync-mode)

 (defun my/org-roam-delete-node-and-replace-links-with-title-stepwise ()
   "Delete an Org-roam node and interactively replace each link to it with plain text."
   (interactive)
   (require 'org-roam)
   (require 'org-id)

   ;; Step 1: Choose the node to delete
   (let* ((node-to-delete (org-roam-node-read))
          (id (org-roam-node-id node-to-delete))
          (title (org-roam-node-title node-to-delete))
          (file (org-roam-node-file node-to-delete))
          (link (concat "id:" id))
          (link-regex
           (format "\\[\\[\\(%s\\)\\(?:\\[.*?\\]\\)?\\]\\]"
                   (regexp-quote link)))
          (org-files (org-roam-list-files)))

     (dolist (f org-files)
       (let ((buf (find-file-noselect f)))
         (with-current-buffer buf
           (goto-char (point-min))
           (while (re-search-forward link-regex nil t)
             (let* ((match-start (match-beginning 0))
                    (match-end (match-end 0))
                    (match-str (match-string 0))
                    (replacement (or title "UNKNOWN"))
                    (ov (make-overlay match-start match-end)))
               (overlay-put ov 'face 'highlight)
               (goto-char match-start)
               (switch-to-buffer buf)
               (recenter)
               (redisplay)

               (if (yes-or-no-p
                    (format "Replace link '%s' with '%s'? "
                            match-str
                            replacement))
                   (progn
                     (delete-region match-start match-end)
                     (goto-char match-start)
                     (insert replacement))
                 (message "Skipped."))

               (delete-overlay ov)))
           (save-buffer))))

     ;; Step 3: Ask to delete the original node's file after processing all links
     (when (and file (file-exists-p file))
       (if (yes-or-no-p (format "Delete file '%s'? " file))
           (progn
             (delete-file file)
             (message "Deleted: %s" file))
         (message "File not deleted."))))))

(use-package
 deft
 :after org
 :bind* (("C-c n d" . deft))
 :config
 (setq deft-recursive t)
 (setq deft-use-filter-string-for-filename t)
 (setq deft-default-extension "org")
 (setq deft-directory my-roam-notes))

(use-package
 org-pdfview
 :after org
 :config
 (add-to-list
  'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))

(setq safe-local-variable-values
      (append
       safe-local-variable-values
       '((eval .
               (progn
                 (my/org-auto-sort-tags-mode 1)
                 (add-hook 'before-save-hook #'my/org-format-sh-blocks nil t)
                 (add-hook 'before-save-hook
                           (lambda () (save-excursion (org-align-tags t)))
                           nil
                           'local)
                 (add-hook 'before-save-hook #'my/org-normalize-header-spacing
                           nil
                           'local)))
         (org-after-todo-statistics-hook . my/org-summary-todo))))

(provide 'org-configuration)
;;; org-configuration.el ends here
