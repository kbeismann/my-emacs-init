;;; init.el --- Emacs initialization file -*- lexical-binding: t; coding: utf-8 -*-


;; Copyright (C) 2019 Karsten E. Beismann

;; Author: Karsten Beismann
;; Homepage: https://github.com/kbeismann/emacs-init
;; Created: Tue Sep 24 21:43:39 2019 +0200
;; Package-Requires: ((emacs "26.3"))
;; Keywords: init emacs


;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; For a full copy of the GNU General Public License see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This Emacs initialization file creates my personalized version of GNU
;; Emacs: a non-invasive editor with a minimalist design.  It highlights
;; relevant information only while hiding non-essential elements whenever
;; possible.  Currently, it uses "leaf" to create a modular system.  Dependent
;; on the system, the bitmap fonts have to be installed and configured
;; upfront.  Note that Pango removed support for bitmap fonts with version
;; 1.33.

;; Some optional settings are not part of this repository.


;;; Code:

;; Require early-init.el.
(let ((early-init-f (expand-file-name
                     "early-init.el"
                     user-emacs-directory)))
  (add-to-list
   'load-path
   early-init-f)
  (require 'early-init))

;; This block effectively disables garbage collection for the initialization
;; time and re-enables it after.  If the machine has enough RAM, at most 64MB
;; every time you start up a new Emacs, this will reduce package-initialize
;; time to about half.

(prog1
    "More generous `gc-cons-threshold' value."
  (setq garbage-collection-messages t)
  (defvar better-gc-cons-threshold 134217728
    "The default value to use for `gc-cons-threshold'.  Currently
128MB.  If you experience stuttering, increase this.")
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (setq gc-cons-threshold
           better-gc-cons-threshold)
     (setq file-name-handler-alist
           file-name-handler-alist-original)
     (makunbound
      'file-name-handler-alist-original)))
  ;; Auto GC.
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (if (boundp 'after-focus-change-function)
         (add-function
          :after after-focus-change-function
          (lambda ()
            (unless (frame-focus-state)
              (garbage-collect))))
       (add-hook
        'after-focus-change-function
        'garbage-collect))
     (defun gc-minibuffer-setup-hook ()
       (setq gc-cons-threshold
             (* better-gc-cons-threshold 2)))
     (defun gc-minibuffer-exit-hook ()
       (garbage-collect)
       (setq gc-cons-threshold
             better-gc-cons-threshold))
     (add-hook
      'minibuffer-setup-hook
      #'gc-minibuffer-setup-hook)
     (add-hook
      'minibuffer-exit-hook
      #'gc-minibuffer-exit-hook)))
  ;; Let's increase the max-lisp-eval-depth and max-specpdl-size to
  ;; prevent exceeding recursion limits.
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 10000)
  ;; Disable certain byte compiler warnings to cut down on the noise.
  (setq byte-compile-warnings '(not cl-functions obsolete)))

;; This hook returns the loading time after startup.  A hook is used so the
;; message does not get clobbered with other messages.
(prog1 "Add timestamp to messages"
  (defun my-message-with-timestamp (old-func fmt-string &rest args)

    "Prepend current timestamp (with microsecond precision) to a
message.  Source:
https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer"
    (apply old-func
           (concat
            (format-time-string "[%F %T.%3N %Z] ")
            fmt-string)
           args))

  ;; Add time stamp to the message function.  This is deactivated at the end
  ;; of the init file.
  (advice-add 'message :around #'my-message-with-timestamp))

(prog1 "Show startup time"
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time
                                               before-init-time)))
                       gcs-done))))


;; Work-related proxy settings.
(let ((proxies "~/gitdir/my-git/my-work-dirs/proxies.el"))
  (if (file-exists-p proxies)
      (progn
        (message "%s" "Found work-related proxy settings...")
        (load proxies))
    (message "%s" "No proxy settings found.")))

;; Setup up leaf and install if necessary.
(prog1 "Use leaf and straight to simplify package management"
  ;; Add archives and assign priorities.
  (setq package-check-signature 'allow-unsigned
                                        ; Do/don't check sig.
        package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                           ("org"          . "https://orgmode.org/elpa/")
                           ("melpa"        . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/"))
        package-archive-priorities '(("gnu"          . 2)
                                     ("org"          . 1)
                                     ("melpa"        . 3)
                                     ("melpa-stable" . 0)))

  (prog1 "Set up straight."
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
    (setq straight-use-package-by-default t)
    (setq straight-vc-git-default-protocol 'https)
    (setq straight-vc-git-force-protocol nil))

  ;; Initialize package BEFORE installing/loading leaf.
  (package-initialize)
  ;; Install leaf if necessary.
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (straight-use-package 'diminish))

  (leaf leaf
    :config
    (leaf leaf-keywords
      :require t)
    (leaf diminish
      :require t)
    (leaf-keywords-init)))

;; Defines a number of directories and files in ~/.emacs.d/.
(leaf *basic-variables
  :doc "Basic variables"
  :config
  (defvar my-custom-file
    (concat user-emacs-directory ".custom.el")
    "My customization file.")
  (defvar my-autosave-dir
    (concat user-emacs-directory "autosave/")
    "My auto-save directory.")
  (defvar my-backup-dir
    (concat user-emacs-directory "backup/")
    "My backup directory.")
  (defvar my-cache-dir
    (concat user-emacs-directory "cache/")
    "My storage area (cache) directory.")
  (defvar my-abbrev-dir
    (concat user-emacs-directory "abbrev/")
    "My abbreviations directory.")
  (defvar my-gitdir
    "~/gitdir/my-git/"
    "My directory for git repositories.")
  (defvar my-library
    (concat my-gitdir "library/")
    "My library repository.")
  (defvar my-bibliography
    (concat my-library "bibliography.bib")
    "My bibliography.")
  (defvar my-readings
    (concat my-gitdir "my-readings/readings.org")
    "My list of readings.")
  (defvar my-init
    (concat my-gitdir "my-emacs-init/")
    "My Emacs initialization file repository.")
  (defvar my-org-templates
    (concat my-init "templates.el")
    "My Org templates.")
  (defvar my-notes-dir
    (concat my-gitdir "my-notes/")
    "My directory for git repositories.")
  (defvar my-notes
    (concat my-notes-dir "notes.org")
    "My notes.")
  (defvar my-todos
    (concat my-notes-dir "notes.org")
    "My to-do list.")
  (defvar my-data-science-readings
    (concat my-gitdir "my-data-science-readings/readings.org"))
  ;; (defvar my-mu4e-setup
  ;;   (concat my-gitdir "mu4e-setup/mu4e-setup.el")
  ;;   "My mu4e file.")
  (defvar my-font-huckleberry
    "Dina:pixelsize=13"
    ;; "Hack:pixelsize=14"
    "My default font for Huckleberry.")
  (defvar my-font-family-huckleberry
    "Dina"
    ;; "Hack"
    "My default font family for Huckleberry.")
  (defvar my-font-arch
    ;; "DejaVu Sans Mono-10"; Arch
    ;; "Hack:pixelsize=12"
    ;; "Terminus:pixelsize=12"
    "Dina:pixelsize=13"
    "My default font for Arch Linux.")
  (defvar my-font-family-arch
    ;; "DejaVu"
    ;; "Hack"
    ;; "Terminus"
    "Dina"
    "My default font family for Arch Linux.")
  (defvar my-font-ubuntu
    ;; "--terminus-medium-r-normal--16.5-120-*-*-*-*-*-*"
    "Terminus:pixelsize=14"
    ;; "Hack:pixelsize=14"
    "My default font setting for Ubuntu.")
  (defvar my-font-family-ubuntu
    "Terminus"
    ;; "Hack"
    "My default font family setting for Ubuntu.")
  (defvar my-max-columns
    78
    "My predefined characters per line (CPL) limit.")
  (defvar path-to-my-snippets
    (concat my-gitdir "my-emacs-init/snippets/")
    "Path to custom snippets.")
  (defvar path-to-snippets
    (concat user-emacs-directory "snippets/")
    "Path to snippets."))

(leaf yasnippet
  :ensure t
  :straight t
  :diminish yas-minor-mode
  :bind
  (("C-c y i"       . yas-insert-snippet)
   ("C-c y v"       . yas-visit-snippet-file))
  :custom
  ((yas-indent-line . 'fixed)
   (yas-global-mode . t))
  :config
  (leaf yasnippet-snippets
    :ensure t)
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 (list path-to-my-snippets))))

(leaf *basic-settings
  :bind
  (("M-o"     . nil)                    ; Unbind face menu.
   ("C-x C-z" . nil)                    ; Unbind suspend frame.
   ("S-SPC"   . just-one-space))        ; Bind just-one-space.
  :setq
  ;; Better splitting behavior.
  (split-height-threshold . 80)
  (split-width-threshold  . '(* 2 my-max-columns))
  :config

  (defalias 'yes-or-no-p 'y-or-n-p)
                                        ; y/n instead of yes/no.
  :custom

  ((user-full-name . "Karsten Beismann")
   ;; Misc. settings.
   (ring-bell-function . 'ignore)  ; No annoying bell.
   (inhibit-startup-screen . t)  ; No starting screen.
   (mouse-yank-at-point . t)  ; Paste at cursor, not at mouse.
   (vc-follow-symlinks . t)  ; Always follow symbolic links.
   (large-file-warning-threshold . 100000000) ; Prevent large file warnings.
   ;; Editing and indentation.
   (tab-width . 4)
                                        ; Default tab width.
   (indent-tabs-mode       . nil)
                                        ; Always indent with spaces.
   (tab-always-indent      . 'complete)
                                        ; Tab indents before completion .
   (next-line-add-newlines . t)
                                        ; New line when C-n.
   (fill-column            . my-max-columns)
                                        ; Set M-q columns.
   ;; Better scrolling behavior.
   (scroll-margin                   . 0)
   (scroll-conservatively           . 10000)
   (scroll-preserve-screen-position . nil)
   (auto-window-vscroll             . nil)
   ;; Cleaner visuals, max. decoration.
   (line-spacing                 . nil)
   (truncate-lines               . t)
   (font-lock-maximum-decoration . t)
   (diff-font-lock-syntax        . t)
   (fringe-mode                  . 1)
                                        ; This is the value for "minimal".
   (global-hl-line-mode          . 1)
   ;; Clipboard behavior.
   (x-select-enable-clipboard-manager . t)
   ;; Debugging.
   (debug-on-error  . t)
   (init-file-debug . t)
   ;; Save-related settings.
   (save-place-mode   . t)
   (desktop-save-mode . nil)
   (blink-cursor-mode . t)
   ;; History.
   (history-length            . 1000)
   (history-delete-duplicates . t)
   ;; Better interpreter settings: scroll down with input/output.
   (comint-scroll-to-bottom-on-input  . t)
   (comint-scroll-to-bottom-on-output . t)
   (comint-move-point-for-output      . t)
   (scroll-down-aggressively          . 0.5)))

                                        ; Not sure what this does.

;; The following snippet checks if a file specified in my-custom-file exists.
;; If it does, set it as custom-file and load it.  If it does not, create the
;; file with "touch", set it as custom-file, and load it.
(leaf *cus-edit
  :doc "Use an external customization file to avoid cluttering this file."
  :config

  (prog1 (message "%s"
                  (concat
                   "Looking for a customization file: "
                   my-custom-file))
    (when (not (file-exists-p my-custom-file))
      (progn
        (message "%s" "No customization file found, creating empty file...")
        (eshell-command
         (concat "touch " my-custom-file))
        (message "%s" "No customization file found, creating empty file...done")))
    (if (file-exists-p my-custom-file)
        (progn
          (message "%s" "Customization file found")
          (setq custom-file my-custom-file)
          (load custom-file))

      (message "%s" "ERROR: Cannot find customization file"))))

(leaf warnings
  :doc "Deal with warnings"
  :config

  (setq warning-suppress-types '((yasnippet backquote-change))))

(leaf *file-settings
  :doc "Backups and more"
  :config

  (leaf autorevert
    :doc "Revert buffers when files change on disk"
    :custom
    ((auto-revert-interval    . 5)
     (global-auto-revert-mode . t)))

  (leaf abbrev
    :diminish abbrev-mode
    :custom
    ((save-abbrevs     . 'silently)
     (abbrev-file-name . my-abbrev-dir)))

  (leaf *lock-files
    :custom
    (create-lockfiles . nil))

  (leaf files
    :custom
    ((require-final-newline  . t)
     (make-backup-files      . t)
     (backup-by-copying      . t)
                                        ; Don't clobber symlinks.
     (kept-new-versions      . 2)
     (kept-old-versions      . 2)
     (version-control        . t)
     (delete-old-versions    . t)
     (backup-directory-alist .
                             `(("."                     . ,my-backup-dir)
                               (,tramp-file-name-regexp . nil)))))

  (leaf *auto-save-files
    :custom
    ((auto-save-default              . t)
     (auto-save-timeout              . 15)
     (auto-save-interval             . 60)
     (auto-save-list-file-prefix     . my-autosave-dir)
     (auto-save-file-name-transforms .
                                     `((".*" ,(file-name-as-directory
                                               my-autosave-dir)
                                        t))))))

;; Font and frame settings, dependent on the OS.
;; TODO: Maybe write a nicer function to avoid repetition.
(leaf *os-related-settings
  :doc "Check OS and set appropriate font and frame size"
  :custom
  ((checkos0 . "Checking OS and hostname...")
   (font0    . "Looking for font family...")
   (font1    . "Setting font..."))
  :config

  (progn
    (message checkos0)
    (if (eq system-type 'gnu/linux)
        (progn
          (message
           (concat checkos0 "done"))
          (defvar my-os
            (substring
             (shell-command-to-string "lsb_release -sd")
             0 -1))
          (message "Found GNU/Linux distribution: %s" my-os)
          (defvar my-hostname
            (substring
             (shell-command-to-string "hostname")
             0 -1))
          (message "Found hostname: %s" my-hostname)
          ;; Font for Huckleberry.
          (if (string-equal "huckleberry"
                            (substring my-hostname 0 11))
              (progn
                (message "Current font settings for Huckleberry: %s"
                         my-font-huckleberry)
                (message font0)
                (if (and
                     (null (string= ""
                                    (shell-command-to-string
                                     "which fc-list")))
                     (null (string= ""
                                    (shell-command-to-string
                                     (concat
                                      "fc-list "
                                      my-font-family-huckleberry)))))
                    (progn
                      (message
                       (concat font0 "done"))
                      (message "Font installed: %s"
                               my-font-family-huckleberry)
                      (add-to-list 'default-frame-alist
                                   `(font . ,my-font-huckleberry)))

                                        ; Works for emacsclient as well.
                  (message "Missing font family: %s" my-font-family-huckleberry)))

            ;; Font for Arch.
            (if (string-equal "Arch"
                              (substring my-os 1 5))
                (progn
                  (message "Current font settings for Arch Linux: %s"
                           my-font-arch)
                  (message font0)
                  (if (and
                       (null
                        (string= ""
                                 (shell-command-to-string
                                  "which fc-list")))
                       (null
                        (string= ""
                                 (shell-command-to-string
                                  (concat
                                   "fc-list "
                                   my-font-family-arch)))))
                      (progn
                        (message
                         (concat font0 "done"))
                        (message "Font installed: %s"
                                 my-font-family-arch)
                        (message font1)
                        (add-to-list 'default-frame-alist
                                     `(font . ,my-font-arch))
                                        ; Works for emacsclient as well.
                        (message
                         (concat font1 "done")))

                    (message "Missing font family: %s" my-font-family-arch)))

              ;; Font for Ubuntu.
              (if (string-equal
                   (substring my-os 0 5)
                   (substring "Ubuntu" 0 5))
                  (progn
                    (message "Current font settings for Ubuntu: %s" my-font-ubuntu)
                    (message font1)
                    (if (and
                         (null
                          (string= ""
                                   (shell-command-to-string
                                    "which fc-list")))
                         (null
                          (string= ""
                                   (shell-command-to-string
                                    (concat
                                     "fc-list "
                                     my-font-family-ubuntu)))))
                        (progn
                          (message "Font installed: %s"
                                   my-font-family-ubuntu)
                          (message font1)
                          (add-to-list 'default-frame-alist
                                       `(font . ,my-font-ubuntu))
                          (message
                           (concat font1 "done")))

                                        ; Works for emacsclient as well.
                      (message "Missing font family: %s" my-font-family-ubuntu))
                    (message "Adjusting frame parameters...")
                    (add-to-list 'default-frame-alist
                                 '(height . 50))
                    (add-to-list 'default-frame-alist
                                 '(width  . 180))
                    (message "Adjusting frame parameters...done"))

                (message "No predefined font settings found")))))

      (message "No Linux-based system found > font settings are not applicable"))))

(leaf *line-numbering
  :doc "The display-line-numbers colors can be changed by editing base16.el"
  :custom
  ((display-line-numbers       . nil)
                                        ; No line numbers (prog-mode only).
   (display-line-numbers-width . 4)
                                        ; Default width.
   (display-line-numbers-widen . t))	; Don't disregard narrowing.
  :config
  ;; Only enable line numbers in prog-mode.
  (progn
    (add-hook 'prog-mode-hook #'display-line-numbers-mode)
    (add-hook 'conf-mode-hook #'display-line-numbers-mode)))

(leaf *misc-functions
  :config

  ;; Using the shell to insert the date.
  (defun insert-current-date-time ()

    "Insert the current date and time in a standard Emacs format."
    (interactive)
    (insert
     (format-time-string "<%Y-%m-%d %a %H:%M>")))

  (global-set-key
   (kbd "C-c d t i")
   'insert-current-date-time)

  (defun insert-current-date ()

    "Insert the current date in a standard Emacs format."
    (interactive)
    (insert
     (format-time-string "<%Y-%m-%d %a>")))

  (global-set-key
   (kbd "C-c d i")
   'insert-current-date)

  ;; Find non ASCII characters.
  (defun find-first-non-ascii-char ()

    "Find the first non-ASCII character from point onward."
    (interactive)
    (let (point)
      (save-excursion
        (setq point
              (catch 'non-ascii
                (while (not (eobp))
                  (or
                   (eq
                    (char-charset
                     (following-char))
                    'ascii)
                   (throw 'non-ascii
                          (point)))
                  (forward-char 1)))))
      (if point
          (goto-char point)
        (message "No non-ASCII characters."))))

  (global-set-key
   (kbd "C-S-s")
   'find-first-non-ascii-char))

(leaf *misc-tools
  :config

  (leaf undo-tree
    :ensure t
    :diminish undo-tree-mode
    :bind
    ("C-c u t" . undo-tree-visualize)
    :custom
    ((global-undo-tree-mode     . t)
     (undo-tree-visualizer-diff . t))))

(leaf *mode-line-settings
  :config

  ;; These options have to be included in mode-line-format as well.
  (column-number-mode 1)
                                        ; Show column number.
  (line-number-mode 1)
                                        ; Show line number in mode line.
  ;; Simplify the cursor position: No proportional position (percentage) nor
  ;; texts like "Bot", "Top" or "All".  Source:
  ;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/
  (setq mode-line-position
        '(;; %p print percent of buffer above top of window, o Top, Bot or All.
          ;; (-3 "%p")
          ;; %I print the size of the buffer, with kmG etc.
          ;; (size-indication-mode ("/" (-4 "%I")))
          ;; " "
          ;; %l print the current line number.
          ;; %c print the current column.
          (line-number-mode
           ("%l"
            (column-number-mode ":%c"))))))

(leaf auto-compile
  :ensure t
  :custom
  (load-prefer-newer . t)
  :config

  (auto-compile-on-load-mode)

  (auto-compile-on-save-mode))

(leaf paradox
  :doc "Nicer package menu"
  :ensure t
  :config
  (paradox-enable))

(leaf *dired-setup
  :config

  (leaf dired
    :commands dired
    :hook
    (dired-mode-hook . dired-hide-details-mode)
    :custom
    ((dired-dwim-target                       . t)
                                        ; Better target.
     (dired-recursive-copies                  . 'always)
                                        ; Copy recursively.
     (dired-recursive-deletes                 . 'always)
                                        ; Delete recursively.
     (dired-hide-details-hide-symlink-targets . nil)
                                        ; Show symlinks.
     (dired-listing-switches                  . "-lahgF --group-directories-first")
     (delete-by-moving-to-trash               . t)))

  (leaf dired-du
    :ensure t
    :require t
    :diminish dired-du-mode
    :custom
    (dired-du-size-format . t))

  (leaf dired-git-info
    :ensure t
    :hook
    (dired-after-readin-hook . dired-git-info-auto-enable)
    :bind
    (dired-mode-map
     (")" . dired-git-info-mode)))

  (leaf dired-subtree
    :ensure t
    :require t
    :bind
    (dired-mode-map
     (";" . dired-subtree-toggle)
     ("'" . dired-subtree-remove))
    :custom
    ((dired-subtree-use-backgrounds . nil)
     (dired-subtree-line-prefix     . "   |-"))))

(leaf tramp
  :custom
  ((tramp-debug-buffer   . t)
   (tramp-read-passwd    . t)
   (tramp-default-method . "ssh")
   (tramp-verbose        . 10))
  :config

  (leaf tramp-term
    :ensure t))

(leaf async
  :ensure t
  :diminish dired-async-mode
  :config

  (dired-async-mode 1)

  (async-bytecomp-package-mode 0))

                                        ; Not sure if this creates issues.

;; Check out https://github.com/jcf/emacs.d/blob/master/init-packages.org.
(leaf eshell
  :commands eshell
  :bind
  ("C-z" . eshell)
  :hook
  (eshell-mode-hook . my-eshell-remove-pcomplete)
  :config

  ;; Fixes weird issues in eshell.
  ;; TODO: Eshell is still using autocomplete.
  (defun my-eshell-remove-pcomplete ()
    (remove-hook 'completion-at-point-functions
                 #'pcomplete-completions-at-point t)))

(leaf *lisp/emacs-lisp
  :config

  (leaf eldoc
    :doc "Show function arglist or variable docstring in echo area"
    :diminish eldoc-mode
    :custom ((eldoc-idle-delay . 0.2))))

(leaf bibtex
  :ensure org-ref
  :ensure gscholar-bibtex
  :bind
  (bibtex-mode-map
   ("C-c C-c" . org-ref-clean-bibtex-entry)
   ("C-c ["   . crossref-lookup)
   ("C-c ]"   . gscholar-bibtex))
  :custom
  ((bibtex-autokey-additional-names     . "_etal")
   (bibtex-autokey-name-separator       . "_")
   (bibtex-autokey-names                . 1)
   (bibtex-autokey-names-stretch        . 1)
   (bibtex-autokey-name-length          . 10)
   (bibtex-autokey-name-year-separator  . "-")
   (bibtex-autokey-year-length          . 4)
   (bibtex-autokey-year-title-separator . "-")
   (bibtex-autokey-titleword-separator  . "_")
   (bibtex-autokey-titlewords           . 3)
   (bibtex-autokey-titlewords-stretch   . 1)
   (bibtex-autokey-titleword-length     . 5))
  :config

  (setq bibtex-dialect 'biblatex)

  ;; A good summary: http://www.jonathanleroux.org/bibtex-mode.html.
  (setq bibtex-additional-formatting '(page-dashes whitespace sort-fields))

  (setq bibtex-entry-format (append bibtex-entry-format bibtex-additional-formatting))

  ;; Path to library only set when directory exists.
  (prog1 "Set bibliography and library paths."
    (let ((path-to-library my-library))
      (when (file-exists-p path-to-library)
        (setq bibtex-completion-library-path path-to-library)))
    (let ((path-to-bib my-bibliography))
      (when (file-exists-p path-to-bib)
        (setq bibtex-completion-bibliography path-to-bib)))))

(leaf flyspell
  :ensure t
  :diminish flyspell-mode
  :hook
  ((prog-mode-hook .
                   (lambda()
                     (flyspell-prog-mode)))
   (text-mode-hook .
                   (lambda()
                     (flyspell-mode))))
  ;; Deactivate for logs and log editing.
  ;; (log-edit-mode-hook . (lambda() (flyspell-mode -1)))
  ;; (change-log-mode-hook . (lambda() (flyspell-mode -1))))
  :config
  ;; If Hunspell is present, setup Hunspell dictionaries.
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell")
                                        ; Use Hunspell.
          ispell-local-dictionary "en_US"

          ispell-dictionary "en_US"

          ispell-really-hunspell nil
                                        ; Temporary fix for Hunspell 1.7.
          ispell-hunspell-dictionary-alist nil)
    ;; Settings for English, US.
    (add-to-list 'ispell-local-dictionary-alist
                 '("english-hunspell"
                   "[[:alpha:]]"
                   "[^[:alpha:]]"
                   "[']"
                   t
                   ("-d" "en_US")
                   nil
                   iso-8859-1))
    ;; Settings for German, Germany.
    (add-to-list 'ispell-local-dictionary-alist
                 '("deutsch-hunspell"
                   "[[:alpha:]]"
                   "[^[:alpha:]]"
                   "[']"
                   t
                   ("-d" "de_DE")
                   nil
                   iso-8859-1))))

(leaf *helm-setup
  :config

  (leaf helm
    :ensure t
    :ensure helm-tramp
    :require helm-config
    :leaf-defer nil
    :diminish (helm-mode helm-autoresize-mode)
    :bind
    (("M-x"     . helm-M-x)
     ("C-s"     . helm-occur)
     ("C-x b"   . helm-mini)
     ("C-x C-f" . helm-find-files)
     ("M-y"     . helm-show-kill-ring)
     ("C-c h"   . helm-command-prefix)
     ("C-c t h" . helm-tramp)
     (helm-command-map
      ("l"      . helm-locate)
      ("s"      . helm-surfraw)
      ("r"      . helm-regexp)
      ("m"      . helm-multi-files)
      ("a"      . helm-apropos)
      ("i"      . helm-imenu)))
    :init
    ;; Remove old bind for helm-command-map.
    (global-unset-key
     (kbd "C-x c"))
    :custom
    ;; Splitting behavior.
    ((helm-split-window-inside-p                              . t)
     (helm-move-to-line-cycle-in-source                       . nil)
                                        ; If t breaks cycling .
     (helm-autoresize-mode                                    . t)
     ;; Use fuzzy matching when possible.
     (helm-mode-fuzzy-match                                   . t)
     (helm-completion-in-region-fuzzy-match                   . t)
     ;; (helm-display-function . 'helm-display-buffer-in-own-frame)
     (helm-display-buffer-reuse-frame                         . nil)
     (helm-use-undecorated-frame-option                       . t)
     ;; Some helm-tramp settings.
     (helm-tramp-control-master                               . t))
    :config

    ;; Turn on helm-mode.
    (helm-mode 1))

  (leaf helm-flyspell
    :doc "Use helm for Flyspell"
    :ensure t
    :bind
    ("C-c f c" . helm-flyspell-correct)))

(leaf base16-theme
  :ensure t
  :custom
  ;; Change the terminal colors.  Not sure if it works.
  (base16-theme-256-color-source . "colors")
  :config

  ;; Load base16.
  (load-theme 'base16-zenburn 1)

  ;; Replace the name of the theme if necessary.
  (prog1 "Create a variable for each color"
    (defvar base00-prop
      (nth 01 base16-zenburn-colors))
    (defvar base01-prop
      (nth 03 base16-zenburn-colors))
    (defvar base02-prop
      (nth 05 base16-zenburn-colors))
    (defvar base03-prop
      (nth 07 base16-zenburn-colors))
    (defvar base04-prop
      (nth 09 base16-zenburn-colors))
    (defvar base05-prop
      (nth 11 base16-zenburn-colors))
    (defvar base06-prop
      (nth 13 base16-zenburn-colors))
    (defvar base07-prop
      (nth 15 base16-zenburn-colors))
                                        ; White.
    (defvar base08-prop
      (nth 17 base16-zenburn-colors))
                                        ; Pink.
    (defvar base09-prop
      (nth 19 base16-zenburn-colors))
                                        ; Orange.
    (defvar base0A-prop
      (nth 21 base16-zenburn-colors))
                                        ; Yellow.
    (defvar base0B-prop
      (nth 23 base16-zenburn-colors))
                                        ; Green.
    (defvar base0C-prop
      (nth 25 base16-zenburn-colors))
    (defvar base0D-prop
      (nth 27 base16-zenburn-colors))
    (defvar base0E-prop
      (nth 29 base16-zenburn-colors))
    (defvar base0F-prop
      (nth 31 base16-zenburn-colors)))

  ;; Remove the vertical line between windows:
  (set-face-background 'vertical-border base00-prop)

  (set-face-foreground 'vertical-border
                       (face-background 'vertical-border))

  ;; Adjust mode line colors.
  (set-face-background 'mode-line base02-prop)

  (set-face-foreground 'mode-line base04-prop)

  (set-face-background 'mode-line-inactive base01-prop)

  (set-face-foreground 'mode-line-inactive base04-prop)

  ;; Hide the fringe but show linebreak arrows.
  (set-face-attribute 'fringe
                      nil :background base00-prop :foreground base02-prop)

  ;; Look of the current line number.  Here, the background is the color
  ;; of the number.
  (set-face-attribute 'line-number-current-line
                      nil :foreground base08-prop :background base01-prop)

  ;; Look and color of the line numbers.
  (set-face-attribute 'line-number
                      nil :background base00-prop :foreground base02-prop)

  (custom-set-faces
   '(font-lock-keyword-face
     ((t
       (:weight bold))))
   '(font-lock-builtin-face
     ((t
       (:weight bold))))))

;; '(font-lock-function-name-face ((t (:weight bold))))
;; '(font-lock-comment-delimiter-face ((t (:slant italic))))
;; '(font-lock-comment-face ((t (:slant italic))))

(leaf avy
  :doc "Move with the power of your mind and jump to things in
  Emacs tree-style"
  :ensure t
  :after base16-theme
  :bind
  (( "M-SPC"   . avy-goto-char)
   ( "M-S-SPC" . avy-goto-char-2))
  :custom
  ((avy-background  . t)
   (avy-all-windows . t)
   ;; NOT SURE IF THIS IS CORRECT: When non-nil highlight the first decision
   ;; char with avy-lead-face-0.  Do this even when the char is terminating.
   ;; Normally avy-lead-face-0 is only used for the first non-terminating
   ;; decision chars.
   (avy-highlight-first . t))
  :config

  ;; Face used for first non-terminating leading chars.
  (set-face-attribute 'avy-lead-face-0 nil
                      :foreground base0A-prop
                      :background base00-prop
                      :weight 'bold)

  ;; Face used for matched leading chars.  Not sure what this does.
  (set-face-attribute 'avy-lead-face-1 nil
                      :foreground base09-prop
                      :background base00-prop
                      :weight 'bold)

  ;; Face used for leading chars.
  (set-face-attribute 'avy-lead-face-2 nil
                      :foreground base0C-prop
                      :background base00-prop
                      :weight 'bold)

  ;; Face used for the leading chars.
  (set-face-attribute 'avy-lead-face nil
                      :foreground base0E-prop
                      :background base00-prop
                      :weight 'bold
                      :underline t)

  ;; Face for foreground/font during selection: base03.
  (set-face-foreground 'avy-background-face base03-prop))

;; Convenience: Instead of marking the line, M-x and C-x remove the whole line
;; automatically if there is no region.
(leaf whole-line-or-region
  :ensure t
  :diminish (whole-line-or-region-global-mode
             whole-line-or-region-local-mode)
  :config

  (whole-line-or-region-global-mode 1))

;; Provides a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.
(leaf which-key
  :ensure t
  :diminish which-key-mode
  :custom
  ((which-key-idle-delay           . 1)
   (which-key-idle-secondary-delay . 0))
  :config

  (which-key-mode 1))

;; Sources: https://github.com/rejeep/emacs/blob/master/init.el
(leaf parens
  :custom
  ((show-paren-delay . 0.0)
   (show-paren-mode  . t))
  :config

  (leaf smartparens
    :url "https://github.com/conao3/dotfiles/commit/d9c0f0dc55e7c65517b2c9ce8eb01f96a425ffd1#diff-f48385f05c9a82908d8bd23c391bbbd3"
    :ensure t
    :diminish (smartparens-mode smartparens-global-mode)
    :require smartparens-config
    :bind
    ("C-c u s" . sp-unwrap-sexp)
    :custom
    ((sp-highlight-pair-overlay . nil)
     (smartparens-global-mode   . t))))

(leaf highlight-indent-guides
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :ensure t
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  :custom
  ((highlight-indent-guides-method     . 'column)
   (highlight-indent-guides-responsive . 'stack)
   (highlight-indent-guides-delay      . 0)))

(leaf *company-setup
  :config

  (leaf company
    :ensure t
    :ensure company-math
    :diminish company-mode
    :custom
    ((company-dabbrev-downcase          . nil)
     (company-idle-delay                . 0)
     (company-tooltip-align-annotations . t)
     (company-show-numbers              . nil)
     (company-minimum-prefix-length     . 1))
    :config

    (global-company-mode 1)

    ;; Global activation of the Unicode symbol completion.
    (add-to-list 'company-backends 'company-math-symbols-unicode))

  (leaf company-bibtex
    :ensure t
    :after bibtex
    :custom
    ;; The regular expression matching key names alphanumeric characters,
    ;; dashes (-), and underscores (_). This is customizable via:
    (company-bibtex-key-regex . "[[:alnum:]+_]*")
    :config

    ;; Add backend for company-bibtex.
    (add-to-list 'company-backends 'company-bibtex)))

(leaf aggressive-indent
  :url "https://github.com/Malabarba/aggressive-indent-mode"
  :doc "aggressive-indent-mode is a minor mode that keeps your code always indented"
  :ensure t
  :config

  (global-aggressive-indent-mode 1))

;; Make sure that there is a single additional line at the end of the file
;; while saving, also removes all white space at the end of lines.
(leaf whitespace
  :ensure t
  :after base16-theme
  :diminish whitespace-mode
  :hook
  ((before-save-hook  . delete-trailing-whitespace)
   (prog-mode-hook    .
                      (lambda ()
                        (whitespace-mode 1)))
   (text-mode-hook    .
                      (lambda ()
                        (whitespace-mode 1)))
   (org-mode-hook     .
                      (lambda ()
                        (whitespace-mode 0)))
   (message-mode-hook .
                      (lambda ()
                        (whitespace-mode 0))))
  :config

  ;; Set the max. column as defined above and delete trailing lines.
  (setq whitespace-line-column my-max-columns

        delete-trailing-lines t)

  ;; Define whitespace stylization.
  (setq whitespace-style '(face newline lines-tail trailing))

  ;; Change colors of text that exceeds 78 columns.
  (set-face-attribute 'whitespace-line nil
                      :foreground base08-prop
                      :background base00-prop
                      :underline nil))

;; Basic bindings for multiple-cursors.
(leaf multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-c C->"     . mc/mark-all-like-this)))

(leaf flycheck               ; TODO: Structure > Move up to Flyspell and wrap.
  :ensure t
  :diminish (global-flycheck-mode flycheck-mode)
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error))
  :custom
  ((global-flycheck-mode . t)))

(leaf *python-setup
  :config

  (leaf conda
    :ensure t
    :after dired
    :bind
    ("C-c $" . conda-env-activate)
    :custom
    ((conda-anaconda-home       . "~/miniconda3/")
     (conda-env-executables-dir . "condabin"))
    :config

    ;; Interactive shell support, include.
    (conda-env-initialize-interactive-shells)

    ;; Eshell support.
    (conda-env-initialize-eshell))

  (leaf python
    :commands python-mode
    :mode (("\\.py\\'" . python-mode)
           ("\\.wsgi$" . python-mode))
    :custom
    (python-indent-offset . 4)
    :config

    (leaf
      flycheck-pycheckers
      :ensure t
      :after flycheck
      :custom ((flycheck-pycheckers-multi-thread . "true")
               (flycheck-pycheckers-max-line-length . 88)
                                        ; Follow Black guidelines.
               (flycheck-pycheckers-checkers . '(pylint flake8 mypy3 bandit)))
      :config ;; TODO: Add this to :hook.
      (with-eval-after-load
          'flycheck
        (add-hook
         'flycheck-mode-hook
         #'flycheck-pycheckers-setup)))

    (leaf pipenv
      :ensure t
      :hook
      (python-mode-hook . pipenv-mode)
      :init

      (setq pipenv-projectile-after-switch-function
            #'pipenv-projectile-after-switch-extended))


    (leaf python-pytest
      :doc "Great defaults: https://shahinism.com/en/posts/emacs-python-pytest/"
      :ensure t
      :ensure projectile
      :after python
      :bind
      (python-mode-map
       ("C-c t p t" . python-pytest)
       ("C-c t p r" . python-repeat)
       ("C-c t p p" . python-pytest-popup)
       ("C-c t p d" . python-pytest-file)
       ("C-c t p D" . python-pytest-file-dwim)
       ("C-c t p f" . python-pytest-function)
       ("C-c t p F" . python-pytest-function-dwim)
       ("C-c t p l" . python-pytest-last-failed))
      :custom
      (python-pytest-arguments .
                               '("--color"     ; Colored output in the buffer.
                                 "--pdb"       ; Run pdb on failure.
                                 "--verbose")) ; More verbose output.
      ;; "--failed-first"                 ; Run the previous failed tests first.
      ;; "--exitfirst"                    ; Exit after first failure.
      ;; "--maxfail=5"; Exit in 5 continuous failures in a run.
      (python-pytest-pdb-track . t))

    (leaf sphinx-doc
      :ensure t
      :load-path "~/gitdir/my-git/sphinx-doc.el/"
      :hook
      (python-mode-hook . sphinx-doc-mode)
      :custom
      ;; Show all arguments (except "self").
      ((sphinx-doc-all-arguments . t)
       (sphinx-doc-exclude-rtype . t)))

    (leaf python-black
      :ensure t
      :hook
      (python-mode-hook .
                        (lambda()
                          (setq-local whitespace-line-column 88)))
      :custom
      (python-black-macchiato-command . "~/.local/bin/black-macchiato"))

    (leaf py-isort
      :ensure t
      :after python)

    (leaf pyimport
      :ensure t
      :bind
      (python-mode-map
       ("C-c m i" . pyimport-insert-missing)
       ("C-c u r" . pyimport-remove-unused)))

    (leaf python-docstring
      :ensure t
      :hook
      (python-mode-hook . python-docstring-mode))))

(leaf org                         ; FIXME: Band aid > Use :bind at some point.
  :config
  (prog1 "Setting directories without :custom"
    (setq org-directory           my-notes-dir)
    (setq org-default-notes-file  my-notes)
    (setq org-todo-file           my-todos)
    (setq org-agenda-files        (list org-directory
                                        my-data-science-readings
                                        my-readings)))
  (let ((work-notes "~/gitdir/my-git/my-work-dirs/notes.el"))
    (if (file-exists-p work-notes)
        (progn
          (message "%s" "Found work-related notes...")
          (load work-notes))
      (message "%s" "No work-related notes found.")))

  (leaf *org-custom
    ;; :init
    ;; (leaf org-plus-contrib
    ;;   :ensure t)
    :bind
    (("C-c a" . org-agenda)
                                        ; Call org-agenda.
     ("C-c c" . org-capture)
                                        ; Org-capture notes.
     ("C-c l" . org-store-link)
                                        ; Store link.
     (org-mode-map
      ("C-c b" . crossref-add-bibtex-entry)
                                        ; Search/add .bib entries.
      ("C-c i" . org-clock-in)
      ("C-c o" . org-clock-out)
      ("C-c n" . org-narrow-to-subtree)
      ;; ("C-c b" . org-narrow-to-block)
      ("C-c e" . org-set-effort)))
    ;; :preface
    ;; (prog1 "Key bindings for org"
    ;;   (global-set-key (kbd "C-c a") 'org-agenda)
    ;;   (global-set-key (kbd "C-c c") 'org-capture)
    ;;   (global-set-key (kbd "C-c l") 'org-store-link)
    ;;   (global-set-key (kbd "C-c b") 'crossref-add-bibtex-entry))
    :hook
    ;; Align tags when saving.
    (((org-mode-hook before-save-hook)
      . org-align-all-tags)
     ;; Switch to DONE when sub-entries are done.
     (org-after-todo-statistics-hook . org-summary-todo)
     ;; Highlight current line in agenda.
     (org-agenda-mode-hook .
                           (lambda ()
                             (hl-line-mode 1))))
    :custom
    ;; Use relative paths.
    ((org-link-file-path-type               . 'relative)
     ;; Startup options.
     (org-startup-indented                  . nil)
     (org-startup-with-latex-preview        . t)
     (org-startup-align-all-tables          . t)
     ;; Indentation.
     (org-indent-mode-turns-on-hiding-stars . nil)
     (org-adapt-indentation                 . nil)
     ;; Misc.
     (org-src-window-setup                  . 'other-window)
     (org-tags-column                       . 70)
     (org-image-actual-width                . nil)
     (org-highlight-latex-and-related       . '(latex script entities))
     (org-catch-invisible-edits             . t)
     ;; All child tasks have to be "DONE" before the parent is "DONE."
     (org-enforce-todo-dependencies         . t)
     ;; To-do settings.
     (org-hierarchical-todo-statistics      . nil)
     ;; Logging.
     (org-log-done-with-time                . t)
     (org-log-done                          . 'time)
     (org-log-repeat                        . 'time)
     ;; Agenda settings.
     (org-agenda-skip-scheduled-if-done     . t)
     (org-agenda-skip-deadline-if-done      . t)
     (org-agenda-include-deadlines          . t)
     (org-agenda-include-diary              . nil)
     ;; (org-agenda-block-separator        . nil)
     (org-agenda-compact-blocks             . t)
     (org-agenda-start-with-log-mode        . nil)
     ;; Better calendar settings: Include last week only if today is Monday,
     ;; always show three weeks. and always start the week on Monday.
     ;;
     (calendar-week-start-day               . 1)
     ;; (org-agenda-start-day                  . "-7d")
     ;; (org-agenda-start-on-weekday           . nil)
     ;; (org-agenda-span                       . 14)
     ))

  ;; Always insert blank line before headings.
  (setq org-blank-before-new-entry '((heading         . auto)
                                     (plain-list-item . auto)))

  (leaf *org-refile
    :custom
    (org-refile-use-outline-path . 'full-file-path)
    (org-outline-path-complete-in-steps . nil)
    (org-refile-allow-creating-parent-nodes . 'confirm)
    (org-refile-targets . '((nil :maxlevel              . 9)
                            (org-agenda-files :maxlevel . 9))))

  (leaf *org-capture-templates
    :doc "Templates for org-capture"
    :config

    ;; If the directory exists, load my templates.
    (let ((templates my-org-templates))
      (if (file-exists-p templates)
          (progn
            (message "%s" "Adding templates...")
            (load templates))
        (message "%s" "No templates specified.")))

    ;; If the directory exists, load templates for work.
    (let ((templates "~/gitdir/work-git/emacs-init/templates.el"))
      (if (and (file-exists-p templates)
		       (boundp 'org-capture-templates))
          (progn
            (message "%s" "Adding templates for work...")
            (load templates))
        (message "%s" "No work-related templates specified."))))

  (leaf *org-remove-tags
    :doc "Clean tags in org mode."
    :url "https://fuco1.github.io/2017-05-09-Automatically-remove-inherited-tags-from-tasks-after-refiling.html"
    :config

    (defun my-org-remove-inherited-local-tags ()

      "Remove local tags that can be inherited instead."
      (let* ((target-tags-local (org-get-tags nil 'local))

             ;; We have to remove the local tags otherwise they would not
             ;; show up as being inherited if they are present on
             ;; parents---the local tag would "override" the parent
             (target-tags-inherited (unwind-protect
                                        (progn
                                          (org-set-tags nil)
                                          (org-get-tags))

                                      (org-set-tags target-tags-local))))
        (-each target-tags-local
          (lambda (tag)
            (when (member tag target-tags-inherited)
              (org-toggle-tag tag 'off))))))

    (add-hook 'org-after-refile-insert-hook 'my-org-remove-inherited-local-tags))

  (leaf *org-summary-todo
    :doc "Switch entry to DONE when all subentries are done, to TODO otherwise."
    :config

    (defun org-summary-todo (n-done n-not-done)

      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done-with-time
            org-log-states)
                                        ; turn off logging
        (org-todo
         (if (= n-not-done 0)
             "DONE" "TODO")))))

  ;; Don't confirm before evaluating.
  (setq org-confirm-babel-evaluate nil)
  ;; Available languages: https://orgmode.org/org.html#Languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (emacs-lisp . t)
     (org        . t)
     (python     . t)
     (R          . t)
     (latex      . t)))
  ;; Use Python 3
  (setq org-babel-python-command "python3")
  ;; Better source block behavior.
  (setq org-src-preserve-indentation t

        org-edit-src-content-indentation 0)
  ;; Highlight code in code blocks in native language, also use TAB as
  ;; in native language.
  (setq org-src-fontify-natively t

        org-src-tab-acts-natively t)
  ;; Change font size for LaTeX previews.
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :html-scale 1.5))
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (leaf org-ref
    :ensure t
    ;; :bind
    ;; (org-mode-map
    ;;  ("C-c i c" . org-ref-helm-insert-cite-link)
    ;;  ("C-c i r" . crossref-lookup))
    :init
    (prog1 "Set paths to bibliography files."
      (setq reftex-use-external-file-finders t)
                                        ; Use this to find bibliographies.
      (setq reftex-external-file-finders
            '(("tex" . "/usr/bin/kpsewhich -format=.tex %f")
              ("bib" . "/usr/bin/kpsewhich -format=.bib %f")))
      (setq reftex-default-bibliography '("~/gitdir/my-git/library/bibliography.bib"))
      (setq org-ref-default-bibliography '("~/gitdir/my-git/library/bibliography.bib")

            org-ref-bibliography-notes "~/gitdir/my-git/library/notes.org"

            org-ref-pdf-directory "~/gitdir/my-git/library/archive/")
      ;; Add "bibtex %b" to enable bibtex export.
      ;; Source: https://github.com/jkitchin/org-ref
      ;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
      (setq org-latex-pdf-process (list "latexmk -pdflatex=lualatex -shell-escape -bibtex -f -pdf %f"))))

  (leaf org-super-agenda
    :ensure t
    :after org
    :custom
    (org-agenda-include-deadlines . t)
    ;; org-agenda-block-separator nil
    (org-agenda-compact-blocks . t)
    ;; org-agenda-start-with-log-mode t
    :config
    (org-super-agenda-mode t)

    (let ((work-agenda "~/gitdir/my-git/my-work-dirs/agenda.el"))
      (if (file-exists-p work-agenda)
          (progn
            (message "%s" "Found work-related agenda settings...")
            (load work-agenda))
        (progn
          (message "%s" "No work-related agenda settings found.")
          (setq org-super-agenda-groups
                '((:name "Bills"
                         :tag "bill"
                         :order 2)
                  (:name "@home"
                         :and (:tag "@home"
                                    :not (:tag "bill"
                                               :tag "shoppinglist"
                                               :tag "reading"
                                               :tag "datascience"
                                               :tag "work"))
                         :order 3)
                  (:name "Data science"
                         :and (:tag "datascience"
                                    :not (:tag "reading"))
                         :order 4)
                  (:name "Data science readings"
                         :and (:tag "datascience"
                                    :tag "reading")
                         :order 5)
                  (:name "Readings"
                         :category "readings"
                         :order 6)
                  (:name "Shopping list"
                         :tag "shoppinglist"
                         :order 7))))))))

(leaf doc-view
  :ensure t
  :config

  (leaf pdf-tools
    :ensure t
    :bind
    (pdf-view-mode-map
     ("C-s" . isearch-forward))
    :init

    (pdf-loader-install)
                                        ; Prepare Emacs for using PDF Tools.
    :custom

    ((pdf-view-display-size                  . 'fit-page)
     (pdf-annot-activate-created-annotations . t))
    :config

    (leaf org-pdfview
      :config

      (add-to-list 'org-file-apps
                   '("\\.pdf\\'" .
                     (lambda (file link)
                       (org-pdfview-open link))))))

  (leaf nov
    :doc "For reading .epub files"
    :ensure t
    :config

    (add-to-list 'auto-mode-alist
                 '("\\.epub\\'" . nov-mode))))

(leaf *git-tools
  :config

  (leaf hl-todo
    :ensure t
    :config

    (global-hl-todo-mode t))

  (leaf magit
    :ensure t
    :after base16-theme
    ;; :diminish magit-auto-revert-mode
    :bind
    (("C-x g"               . magit-status))
    :custom
    (magit-diff-refine-hunk . 'all)
    :config

    ;; (set-face-attribute 'magit-diff-added
    ;;                     nil :background base0B-prop :foreground nil)
    ;; (set-face-attribute 'magit-diff-added-highlight
    ;;                     nil :background base0B-prop :foreground nil)
    ;; (set-face-attribute 'magit-diff-removed
    ;;                     nil :background base09-prop :foreground nil)
    ;; (set-face-attribute 'magit-diff-removed-highlight
    ;;                     nil :background base09-prop :foreground nil)
    (leaf magit-todos
      :ensure t
      :after magit
      :commands (magit-todos-mode)
      :config

      ;; Suppress warning: magit-todos: Not overriding bind of "jT" in
      ;; magit-status-mode-map.
      (let ((inhibit-message t))
        (magit-todos-mode 1))))

  (leaf gitconfig-mode
    :ensure t)

  (leaf gitignore-mode
    :ensure t)

  (leaf gitattributes-mode
    :ensure t)

  (leaf git-timemachine
    :url "https://gitlab.com/pidu/git-timemachine"
    :ensure t
    :diminish git-timemachine-mode)

  (leaf git-auto-commit-mode
    :ensure t
    :diminish git-auto-commit-mode
    :custom
    (gac-automatically-push-p . t)))

;; TODO: Structure > Wrap LSP-related sections.
(leaf lsp-mode
  :ensure t
  :ensure projectile company yasnippet flycheck which-key
  :commands (lsp lsp-deferred)
  :hook
  (python-mode-hook . lsp-deferred)
  :bind
  (lsp-mode-map
   (("C-c r p" . lsp-rename)
    ("C-c f r" . lsp-find-references)
    ("C-c f d" . lsp-find-definition)
    ("C-c w d" . xref-find-definitions-other-window)
    ("C-c d p" . lsp-describe-thing-at-point)))
  :custom
  ((lsp-inhibit-message              . nil)
   (lsp-message-project-root-warning . t)
   ;; Debugging.
   (lsp-log-io                       . nil)
   (lsp-server-trace                 . nil)
   ;; Customization.
   (lsp-enable-symbol-highlighting   . t)
   (lsp-prefer-flymake               . nil)
   (lsp-auto-guess-root              . t)
   (lsp-enable-snippet               . t)
   (lsp-idle-delay                   . 0.1)
   ;; Advanced.
   (lsp-completion-show-detail       . nil)
   (lsp-completion-show-kind         . nil)
   (lsp-eldoc-enable-hover           . nil)
   (lsp-enable-indentation           . t)
   (lsp-headerline-breadcrumb-enable . nil)
   (lsp-enable-on-type-formatting    . nil)
   (lsp-modeline-code-actions-enable . nil)
   (lsp-modeline-diagnostics-enable  . nil))
  :config
  (prog1 "Enable which-key integration in the active major mode for lsp-mode-map."
    (with-eval-after-load
        'lsp-mode
      (add-hook
       'lsp-mode-hook
       #'lsp-enable-which-key-integration)))

  ;; Define faces for highlighting in LSP.
  (set-face-attribute 'lsp-face-highlight-write nil
                      :italic nil
                      :underline nil
                      :inherit 'unspecified
                      :background base02-prop
                      :inverse-video t)

  (set-face-attribute 'lsp-face-highlight-read nil
                      :italic nil
                      :underline nil
                      :inherit 'unspecified
                      :background base02-prop)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ("pyls.plugins.flake8.enabled" t t))))

(leaf helm-lsp
  :after helm lsp-mode
  :commands helm-lsp-workspace-symbol)

(leaf markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init

  (setq markdown-command "pandoc"))

;; Always use GPG2 and use loopback option for better compatibility.
(leaf epa
  :custom
  (epa-pinentry-mode . 'loopback)
  :config
  (leaf epa-config
    :custom
    (epg-gpg-program . "gpg2"))
  (leaf auth-source
    :custom
    (auth-sources .
                  '("~/.authinfo.gpg")))
  (leaf pinentry))

(leaf yaml-mode
  :config
  :ensure t
  :bind
  (yaml-mode-map
   ("\C-m" . newline-and-indent)))

(leaf csv-mode
  :config

  (add-to-list 'auto-mode-alist
               '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t))

(leaf ediff
  :custom
  ((ediff-window-setup-function . 'ediff-setup-windows-plain)
                                        ; Don't start another frame.
   (ediff-split-window-function . 'split-window-horizontally)) ; Put windows side by side.
  :config
  ;; Revert windows on exit - needs winner mode
  (winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(leaf ssh-deploy
  :url "https://github.com/cjohansson/emacs-ssh-deploy"
  :doc "Effortlessly deploy local files and directories to remote hosts via Tramp."
  :ensure t
  :bind
  ("C-c z d" . ssh-deploy-prefix-map)
  :hook
  ((after-save-hook . ssh-deploy-after-save)
   (find-file       . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode)
  (ssh-deploy-add-menu))

(leaf projectile
  :after helm
  :ensure helm-projectile
  :diminish projectile-mode
  :bind
  ("C-c p" . projectile-command-map)
  :custom
  ((projectile-mode              . t)
   (projectile-completion-system . 'helm))
  :config

  (helm-projectile-on))

(leaf centered-window
  :ensure t)

(leaf org-mind-map
  :doc "This is an Emacs package that creates graphviz directed
  graphs from the headings of an org file"
  :require ox-org
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;; :ensure-system-package (gvgen . graphviz)
  :custom
  (org-mind-map-include-text . t)
  ;; (org-mind-map-engine . "dot")  ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (org-mind-map-engine . "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

;; Remove timestamp from messages after startup.
(advice-remove 'message #'my-message-with-timestamp)
;;; Footer:
(provide 'init)
;;; init.el ends here
