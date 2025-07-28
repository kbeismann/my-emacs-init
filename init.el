;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 Karsten E. Beismann

;; Author: Karsten Beismann
;; Homepage: https://github.com/kbeismann/emacs-init
;; Created: Tue Sep 24 21:43:39 2019 +0200

;; This file is part of my Emacs configuration.
;;
;; My Emacs configuration is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my personal Emacs configuration file. The focus is on minimalism,
;; distraction-free editing, and visual clarity.

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package-management)

;; Load work-related settings.
(let ((work-projects (expand-file-name "projects.el" user-emacs-directory)))
  (cond
   ((file-exists-p work-projects)
    (message "Found project-related settings...")
    (load-file work-projects))
   (t
    (message "No project-related settings found."))))

(use-package shfmt)

(use-package elisp-autofmt :commands (elisp-autofmt-mode elisp-autofmt-buffer))

;; Defines a number of directories and files in ~/.emacs.d/.
(defvar my-gitdir (file-truename "~/gitdir/my-git/")
  "My directory for git repositories.")
(defvar my-library (concat my-gitdir "library/")
  "My library repository.")
(defvar my-bibliography (concat my-library "bibliography.bib")
  "My bibliography.")
(defvar my-init (concat my-gitdir "my-emacs-init/")
  "My Emacs initialization file repository.")
(defvar my-default-line-width 80
  "My predefined characters per line (CPL) limit.")
(defvar path-to-my-snippets (concat my-gitdir "my-emacs-init/snippets/")
  "Path to custom snippets.")
(defvar path-to-snippets (concat user-emacs-directory "snippets/")
  "Path to snippets.")

;; Unset and set basic bindings.
(global-unset-key (kbd "M-o")) ; Unbind face menu.
(global-unset-key (kbd "C-x C-z")) ; Unbind suspend frame.
(global-unset-key (kbd "C-z")) ; Unbind suspend in a terminal-context.
(global-set-key (kbd "M-SPC") 'cycle-spacing) ; Richer alternative to just-one-space.

;; Better splitting behavior.
(setq split-height-threshold my-default-line-width)
(setq split-width-threshold (* 2 my-default-line-width))

(defalias 'yes-or-no-p 'y-or-n-p)

(setenv "BASH_ENV" "~/.bashrc")

(setq user-full-name "Karsten Beismann")

;; Miscellaneous settings.
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq mouse-yank-at-point t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold (* 10 1024 1024))
(setq sentence-end-double-space nil)
(setq-default sentence-end "[.?!…‽][]\"')}»›]*[ \t\n]+")

;; Editing and indentation.
(setq tab-always-indent t)
(setq next-line-add-newlines t)
(setq-default fill-column my-default-line-width)

;; Better scrolling behavior.
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 100)
(setq scroll-preserve-screen-position nil)
(setq auto-window-vscroll nil)
(setq next-screen-context-lines 30)

;; Clipboard behavior.
(setq x-select-enable-clipboard-manager t)

;; Debugging.
(setq debug-on-error nil)
(setq init-file-debug t)

;; Save-related settings.
(setq save-place-mode t)
(setq desktop-save-mode nil)

;; History.
(setq history-length 1000)
(setq history-delete-duplicates t)

;; Better interpreter settings: scroll down with input/output.
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; GC tuning for minibuffer interaction.
(defun my/gc-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/gc-minibuffer-exit-hook ()
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my/gc-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/gc-minibuffer-exit-hook)

;; Configure warnings.
(setq warning-suppress-types '((yasnippet backquote-change)))

(use-package
 no-littering
 :defer nil
 :config
 (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
 (setq no-littering-var-directory
       (expand-file-name "var/" user-emacs-directory))
 (setq no-littering-autosave-directory
       (expand-file-name "autosave/" no-littering-var-directory))
 (setq no-littering-backup-directory
       (expand-file-name "backup/" no-littering-var-directory))
 (setq no-littering-abbrev-directory
       (expand-file-name "abbrev/" no-littering-var-directory))

 ;; Configure recentf exclude list.
 (setq recentf-exclude '(no-littering-var-directory))
 (setq recentf-exclude '(no-littering-etc-directory))

 (message "%s" (concat "Looking for a customization file: " custom-file))
 (when (not (file-exists-p custom-file))
   ;; Create an empty customization file.
   (message "%s" "No customization file found, creating empty file.")
   (eshell-command (concat "touch " custom-file))
   (message "%s" "Created empty file."))
 (if (file-exists-p custom-file)
     ;; Load customization file.
     (progn
       (message "%s" "Customization file found.")
       (load custom-file))
   (message "%s" "ERROR: Cannot find customization file."))

 ;; Configure auto-save settings.
 (setq auto-save-default t)
 (setq auto-save-timeout 15)
 (setq auto-save-interval 60)
 (setq auto-save-list-file-prefix no-littering-autosave-directory)
 (setq auto-save-file-name-transforms
       `((".*" ,no-littering-autosave-directory t)))

 (use-package
  abbrev
  :ensure nil
  :straight nil
  :diminish abbrev-mode
  :config
  (setq save-abbrevs 'silently)
  (setq abbrev-file-name no-littering-abbrev-directory)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

 ;; Configure file handling.
 (setq require-final-newline t)
 (setq make-backup-files t)
 (setq backup-by-copying t)
 (setq kept-new-versions 2)
 (setq kept-old-versions 2)
 (setq version-control t)
 (setq delete-old-versions t)
 (setq backup-directory-alist
       `(("." . ,no-littering-backup-directory)
         (,tramp-file-name-regexp . nil)))

 ;; Configure lockfiles.
 (setq create-lockfiles nil))

;; Custom functions.
;; From https://www.emacswiki.org/emacs/UnfillParagraph.
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph.
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'my/unfill-paragraph)

(defun my/copy-git-current-sha ()
  "Copy the current Git commit SHA to the clipboard."
  (interactive)
  (let ((sha (string-trim (shell-command-to-string "git rev-parse HEAD"))))
    (when (string-match-p "^[0-9a-f]\\{40\\}$" sha)
      (kill-new sha)
      (message "Copied SHA: %s" sha))))
(define-key global-map (kbd "C-c c s") 'my/copy-git-current-sha)

(defun my/copy-current-path-to-file ()
  "Copies the path of the current file to the clipboard."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "Copied file path: %s" buffer-file-name))
    (message "No file is currently visiting.")))
(define-key global-map (kbd "C-c c p") 'my/copy-current-path-to-file)

(defun my/go-to-chezmoi-directory ()
  "Go to the Chezmoi configuration directory."
  (interactive)
  (let ((chezmoi-dir (expand-file-name "~/.local/share/chezmoi/")))
    (find-file chezmoi-dir)))
(define-key global-map (kbd "C-c c d") 'my/go-to-chezmoi-directory)

(defun my/insert-current-date-time ()
  "Insert the current date and time in a standard Emacs format."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))
(global-set-key (kbd "C-c d t i") 'my/insert-current-date-time)

(defun my/insert-current-date ()
  "Insert the current date in a standard Emacs format."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))
(global-set-key (kbd "C-c d i") 'my/insert-current-date)

(defun my/find-first-non-ascii-char ()
  "Find the first non-ASCII character from point onward."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char)) 'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ASCII characters."))))
(global-set-key (kbd "C-S-s") 'my/find-first-non-ascii-char)

(defun my/collapse-multiple-blank-lines ()
  "Collapse multiple blank lines into a single blank line in the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (replace-regexp "^[[:space:]]*\n\\(?:[[:space:]]*\n\\)+" "\n"
                      nil (point-min) (point-max)))))

(defun my/add-collapse-to-before-save ()
  "Add `my/collapse-multiple-blank-lines' to the buffer-local `before-save-hook'."
  (add-hook 'before-save-hook #'my/collapse-multiple-blank-lines
            'append
            'local))

;; Add the hook function to org-mode-hook and emacs-lisp-mode-hook.
(add-hook 'emacs-lisp-mode-hook #'my/add-collapse-to-before-save)

(defun my/batch-collapse-blank-lines (directory)
  "Collapse multiple blank lines in all files within DIRECTORY and its subdirectories."
  (interactive "DDirectory to process: ")
  (let ((files (find-lisp-find-files directory "."))
        (processed-count 0))
    (message "Processing files recursively in %s..." directory)
    (dolist (file files)
      (condition-case err
          (progn
            (message "  Processing %s..." (file-relative-name file directory))
            (let ((buffer (find-file-noselect file)))
              (with-current-buffer buffer
                (let ((original-modified-p (buffer-modified-p)))
                  (my/collapse-multiple-blank-lines)
                  (when (buffer-modified-p)
                    (save-buffer)
                    (setq processed-count (1+ processed-count)))
                  (unless original-modified-p
                    (kill-buffer buffer))))))
        (error
         (message "Error processing %s: %s"
                  (file-relative-name file directory)
                  (error-message-string err))))
      (sit-for 0))
    (message "Finished processing files recursively in %s. %d files modified."
             directory
             processed-count)))

(require 'dired-configuration)
(require 'helm-configuration)
(require 'appearance)

(use-package
 treesit-auto
 :commands global-treesit-auto-mode
 :custom (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode t))

(use-package
 undo-tree
 :disabled t
 :after no-littering
 :diminish undo-tree-mode
 :bind (("C-c u t" . undo-tree-visualize))
 :config
 (setq undo-tree-visualizer-diff t)
 (global-undo-tree-mode t))

(use-package
 yasnippet
 :diminish yas-minor-mode
 :bind
 (("C-c y i" . yas-insert-snippet) ("C-c y v" . yas-visit-snippet-file))
 :config
 (add-hook
  'python-base-mode-hook #'(lambda () (yas-activate-extra-mode 'python-mode)))
 (use-package yasnippet-snippets)
 (setq yas-indent-line 'fixed)
 (setq yas-snippet-dirs (append yas-snippet-dirs (list path-to-my-snippets)))
 (yas-reload-all)
 (yas-global-mode))

;; Configure Tramp settings and load tramp-term.
(setq tramp-debug-buffer t)
(setq tramp-read-passwd t)
(setq tramp-default-method "ssh")
(setq tramp-verbose 10)
(use-package tramp-term :after tramp)

(use-package
 flyspell
 :diminish flyspell-mode
 :hook
 ((prog-mode . (lambda () (flyspell-prog-mode)))
  (text-mode . (lambda () (flyspell-mode))))
 :config
 ;; If Hunspell is present, setup Hunspell dictionaries.
 (when (executable-find "hunspell")
   (setq
    ispell-program-name (executable-find "hunspell")
    ispell-local-dictionary "en_US"
    ispell-dictionary "en_US"
    ispell-really-hunspell nil ; Temporary fix for Hunspell 1.7.
    ispell-hunspell-dictionary-alist nil)
   ;; Settings for English, US.
   (add-to-list
    'ispell-local-dictionary-alist
    '("english-hunspell"
      "[[:alpha:]]"
      "[^[:alpha:]]"
      "[']"
      t
      ("-d" "en_US")
      nil
      iso-8859-1))
   ;; Settings for German, Germany.
   (add-to-list
    'ispell-local-dictionary-alist
    '("deutsch-hunspell"
      "[[:alpha:]]"
      "[^[:alpha:]]"
      "[']"
      t
      ("-d" "de_DE")
      nil
      iso-8859-1))))

;; Configure handling parens.
(setq show-paren-delay 0.0)
(setq show-paren-mode t)
(use-package
 smartparens
 :diminish (smartparens-mode smartparens-global-mode)
 :bind* (("C-c u s" . sp-unwrap-sexp))
 :config
 (require 'smartparens-config)
 (smartparens-global-mode t)
 (setq sp-highlight-pair-overlay nil)
 (setq sp-show-pair-from-inside t))

(use-package
 highlight-indent-guides
 ;; From https://github.com/DarthFennec/highlight-indent-guides
 :diminish highlight-indent-guides-mode
 :hook
 ((python-base-mode . highlight-indent-guides-mode)
  (yaml-ts-mode . highlight-indent-guides-mode))
 :config
 (setq highlight-indent-guides-method 'character)
 (setq highlight-indent-guides-responsive 'top)
 (setq highlight-indent-guides-delay 0))

(use-package
 company
 :diminish company-mode
 :config
 (setq company-dabbrev-downcase nil)
 (setq company-idle-delay 0)
 (setq company-tooltip-align-annotations t)
 (setq company-show-numbers nil)
 (setq company-minimum-prefix-length 1)
 (use-package company-math)
 (global-company-mode 1)
 ;; Global activation of the Unicode symbol completion.
 (add-to-list 'company-backends 'company-math-symbols-unicode))

(setq show-trailing-whitespace t)

(use-package
 whitespace
 :diminish whitespace-mode
 :hook
 ((prog-mode . whitespace-mode)
  (markdown-mode . whitespace-mode)
  (org-mode . whitespace-mode))
 :config (setq whitespace-line-column my-default-line-width)
 (setq
  whitespace-style
  '(face ;; Apply the face `whitespace` to detected characters.
    trailing ;; Visualize trailing whitespace.
    empty ;; Visualize leading/trailing whitespace on empty lines or at buffer beginning/end.
    tabs tab-mark big-indent space-before-tab space-after-tab
    missing-newline-at-eof ;; Visualize when the last line is not followed by a newline.
    newline ;; Visualize newlines.
    newline-mark ;; Replace newlines with `whitespace-display-chars`.
    )))

(use-package
 ws-butler
 :diminish ws-butler-mode
 :config
 ;; By default, ws-butler removes trailing whitespace and lines with only
 ;; whitespace. Add modes where you don't want automatic cleaning.
 (add-to-list 'ws-butler-global-exempt-modes 'magit-mode)
 (ws-butler-global-mode t))

;; Basic bindings for multiple-cursors.
(use-package
 multiple-cursors
 :init (setq warning-suppress-types '((files)))
 :bind*
 (("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/mark-all-like-this))
 :config (setq mc/always-run-for-all t))

(use-package
 flycheck
 :diminish (global-flycheck-mode flycheck-mode)
 :bind (("M-n" . flycheck-next-error) ("M-p" . flycheck-previous-error))
 :hook (after-init . global-flycheck-mode))

(defun my-python-mode-settings ()
  "Custom settings for `python-mode'."
  (setq-local python-indent-offset 8)
  (setq-local indent-tabs-mode nil))
(add-hook 'python-ts-mode-hook 'my-python-mode-settings)

(use-package
 sphinx-doc
 :disabled t
 :load-path "~/gitdir/my-git/sphinx-doc.el/"
 :diminish sphinx-doc-mode
 :hook (python-base-mode . sphinx-doc-mode)
 :config
 ;; Show all arguments (except "self").
 (setq sphinx-doc-all-arguments t) (setq sphinx-doc-exclude-rtype t))

(use-package python-docstring :hook (python-base-mode . python-docstring-mode))

(use-package ruff-format)

(use-package
 rust-mode
 :config
 (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
 (setq rust-format-on-save t)
 (add-hook 'rust-mode-hook (lambda () (prettify-symbols-mode))))

(require 'org-configuration)

(use-package
 doc-view
 :config
 (use-package
  pdf-tools
  :bind (:map pdf-view-mode-map ("C-s" . isearch-forward))
  :init (pdf-loader-install)
  :config
  (setq pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)))

;; Git tooling.
:config
;; Fixes temporary issues with vc-mode.
(setq vc-handled-backends ())

(use-package hl-todo :config (global-hl-todo-mode t))

;; Configure Ediff.
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; Don't start another frame.
;; Revert windows on exit - needs winner mode.
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package
 magit
 :diminish magit-auto-revert-mode
 :init
 (require 'helm)
 (require 'smartparens)
 :config
 (magit-auto-revert-mode t)
 (setq magit-diff-refine-hunk 'all)
 (setq magit-log-auto-more t)
 ;; Auto-revert.
 (setq auto-revert-interval 1)
 (setq global-auto-revert-mode nil)

 ;; Custom settings for git-commit-mode.
 (defun my/git-commit-mode-settings ()
   "Custom settings for `git-commit-mode'."
   (setq-local fill-column 72))
 (add-hook 'git-commit-mode-hook #'my/git-commit-mode-settings))

(use-package
 git-timemachine
 ;; https://codeberg.org/pidu/git-timemachine
 :diminish git-timemachine-mode
 :bind (("C-c t m" . git-timemachine)))

(use-package
 git-auto-commit-mode
 :diminish git-auto-commit-mode
 :config (setq gac-automatically-push-p t))

;; Emacs Refactor (EMR) is a framework for providing language-specific
;; refactoring in Emacs.
(use-package
 emr
 :disabled t
 :config (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu))

(use-package
 eglot
 :disabled t
 :hook (python-base-mode . eglot-ensure)
 :bind
 (:map
  eglot-mode-map
  ("C-c e r" . eglot-rename)
  ("C-c e c a" . eglot-code-actions)
  ("C-c d o c" . eldoc)))

(use-package
 lsp-mode
 :commands (lsp lsp-deferred)
 :hook
 ((python-base-mode . lsp-deferred)
  (rust-ts-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))
 :bind (:map lsp-mode-map ("M-?" . lsp-find-references) ("M-." . lsp-find-definition))
 :init
 (setq lsp-keymap-prefix "C-c l")
 (setq lsp-diagnostics-provider :none)
 :config
 (setq lsp-log-io t)
 (setq lsp-file-watch-threshold 10000)
 (setq lsp-restart 'auto-restart)
 (lsp-register-custom-settings
  '(("pylsp.plugins.pylsp_mypy.enabled" nil nil)
    ("pylsp.plugins.pylsp_mypy.live_mode" nil nil)
    ("pylsp.plugins.rope_autoimport.enabled" nil nil)
    ("pylsp.plugins.rope_completion.eager" t t)
    ("pylsp.plugins.pylsp_rope.enabled" t t)
    ("pylsp.plugins.rope_rename.enabled" t t)
    ("pylsp.plugins.jedi_rename.enabled" nil nil)
    ("pylsp.plugins.rope_completion.enabled" t t)
    ("pylsp.plugins.jedi_completion.enabled" nil nil)
    ("pylsp.plugins.pyls_isort.enabled" nil nil)
    ("pylsp.plugins.pyls_memestra.enabled" nil nil)
    ("pylsp.plugins.pyls_flake8.enabled" nil nil)
    ("pylsp.plugins.pyls_black.enabled" nil nil)
    ("pylsp.plugins.yapf.enabled" nil nil)
    ("pylsp.plugins.pylint.enabled" nil nil)
    ("pylsp.plugins.flake8.enabled" nil nil)))
 (use-package
  lsp-pyright
  :after (python lsp)
  :disabled t
  :hook
  (python-base-mode
   .
   (lambda ()
     (require 'lsp-pyright)
     (lsp-deferred)))))

(use-package
 markdown-mode
 :mode ("README\\.md\\'" . gfm-mode)
 :init (setq markdown-command "multimarkdown")
 :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

;; Always use GPG2 and use loopback option for better compatibility.
(use-package
 epa
 :config
 (setq epa-pinentry-mode 'loopback)
 (setq epg-gpg-program "gpg2")
 (use-package pinentry))

(use-package
 yaml-ts-mode
 :mode ("\\.ya?ml\\'" . yaml-ts-mode)
 :hook (yaml-ts-mode . display-line-numbers-mode)
 :config (define-key yaml-ts-mode-map (kbd "C-m") 'newline-and-indent))

(use-package
 csv-mode
 :disabled t
 :config (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
 (autoload 'csv-mode "csv-mode"
   "Major mode for editing comma-separated value files."
   t))

(use-package
 ssh-deploy
 ;; Effortlessly deploy local files and directories to remote hosts via Tramp.
 ;; https://github.com/cjohansson/emacs-ssh-deploy
 :disabled t
 :bind (("C-c z d" . ssh-deploy-prefix-map))
 :hook ((after-save-hook . ssh-deploy-after-save) (find-file . ssh-deploy-find-file))
 :config
 (setq ange-ftp-netrc-filename "~/.authinfo.gpg")
 (ssh-deploy-line-mode)
 (ssh-deploy-add-menu))

(use-package
 projectile
 :diminish projectile-mode
 :bind (("C-c p" . projectile-command-map))
 :init (projectile-mode t)
 :config
 (setq projectile-completion-system 'helm)
 (setq projectile-indexing-method 'alien)
 (add-to-list 'projectile-globally-ignored-files ' "*ediff-merge*"))

(use-package
 json-mode
 :config (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))

(use-package
 dockerfile-mode
 :config
 (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package
 kubernetes
 :bind (("C-c k o" . kubernetes-overview))
 :commands (kubernetes-overview)
 :config (setq kubernetes-redraw-frequency 600))

(require 'ai-configuration)

;;; Footer:
(provide 'init)
;;; init.el ends here
