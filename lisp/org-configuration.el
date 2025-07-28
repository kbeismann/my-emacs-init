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
 :straight
 (:package
  org
  :type git
  :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
  :local-repo "org"
  :depth 1)
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
   ("C-c u l" . #'my/org-roam-unlink-at-point)))
 :hook
 (org-mode
  .
  (lambda ()
    (add-hook 'before-save-hook (lambda () (save-excursion (org-align-tags t)))
              nil
              'local)
    (my/org-auto-sort-tags-mode 1)))
 ;; Switch to DONE when sub-entries are done.
 (org-after-todo-statistics-hook . org-summary-todo)
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
 (setq org-tags-column (- my-default-line-width))
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
 (defun my-org-agenda-unscheduled ()
   "Display org-agenda for custom command 'u' (Unscheduled TODOs)."
   (interactive)
   (org-agenda nil "u"))
 (global-set-key (kbd "C-c o u") 'my-org-agenda-unscheduled)

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
         (with-temp-buffer
           (insert-file-contents file)
           (org-mode)
           (ignore-errors
             (org-align-tags))
           (write-file file)
           (message "Aligned tags in %s" file))))))

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
 (setq org-latex-toc-command "\\tableofcontents \\clearpage"))

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
               (:name "Readings" :category "readings" :tag "reading" :order 4)
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

(provide 'org-configuration)
;;; org-configuration.el ends here
