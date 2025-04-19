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

(let ((work-emacs-dir "~/gitdir/my-git/my-work-emacs-init"))
  (if (file-exists-p work-emacs-dir)
      (prog1 "Load project-related settings."
        (message "%s" "Found project-related settings...")
        (add-to-list 'load-path work-emacs-dir)
        (require 'projects))
    (message "%s" "No project-related settings found.")))

(prog1 "Add archives and assign priorities."
  (setq package-check-signature 'allow-unsigned) ; Do/don't check sig.
  (setq package-archives '(
                                       ("gnu" . "https://elpa.gnu.org/packages/")
                                       ("org" . "https://orgmode.org/elpa/")
                                       ("melpa" . "https://melpa.org/packages/")
                                       ("melpa-stable" . "https://stable.melpa.org/packages/")))
  (setq package-archive-priorities '(
                                     ("gnu" . 2)
                                     ("org" . 1)
                                     ("melpa" . 3)
				     ("melpa-stable" . 0)))

  ;; Initialize package BEFORE installing/loading leaf.
  (package-initialize))

(prog1 "Setting up straight.el."
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
  (setq straight-check-for-modifications nil))

;; Setup up leaf and install if necessary.
(prog1 "Use leaf to simplify package management."
  (setq straight-vc-git-default-clone-depth 1)
  (setq straight-use-package-by-default t)

  (straight-use-package 'use-package)

  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

;; Defines a number of directories and files in ~/.emacs.d/.
(prog1 "Setting basic variables."
  (defvar my-gitdir
    (file-truename "~/gitdir/my-git/")
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
  (defvar my-roam-notes
    (concat my-gitdir "my-roam-notes/nodes/")
    "My Roam notes.")
  (defvar my-todos
    (concat my-notes-dir "notes.org")
    "My to-do list.")
  (defvar my-max-columns
    78
    "My predefined characters per line (CPL) limit.")
  (defvar path-to-my-snippets
    (concat my-gitdir "my-emacs-init/snippets/")
    "Path to custom snippets.")
  (defvar path-to-snippets
    (concat user-emacs-directory "snippets/")
    "Path to snippets."))

(prog1 "Basic configurations."
  (global-unset-key (kbd "M-o")) ; Unbind face menu.
  (global-unset-key (kbd "C-x C-z")) ; Unbind suspend frame.
  (global-unset-key (kbd "C-z"))  ; Unbind suspend in a terminal-context.
  (global-set-key (kbd "M-SPC") 'cycle-spacing) ; Richer alternative to just-one-space.

  ;; Better splitting behavior.
  (setq split-height-threshold 80)
  (setq split-width-threshold (* 2 my-max-columns))

  (defalias 'yes-or-no-p 'y-or-n-p)

  (setenv "BASH_ENV" "~/.bashrc")

  (setq user-full-name "Karsten Beismann")

  ;; Misc. settings.
  (setq ring-bell-function 'ignore)  ; No annoying bell.
  (setq inhibit-startup-screen t)  ; No starting screen.
  (setq mouse-yank-at-point t)  ; Paste at cursor, not at mouse.
  (setq vc-follow-symlinks t)  ; Always follow symbolic links.
  (setq large-file-warning-threshold 100000000) ; Prevent large file warnings.

  ;; Editing and indentation.
  (setq tab-width 4)  ; Default tab width.
  (setq-default indent-tabs-mode nil)  ; Always indent with spaces.
  (setq tab-always-indent t)  ; Tab indents before completion.
  (setq next-line-add-newlines t)  ; New line when C-n.
  (setq fill-column my-max-columns)  ; Set M-q columns.

  ;; Better scrolling behavior.
  (setq scroll-step 1)
  (setq scroll-margin 5)
  (setq scroll-conservatively 100)
  (setq scroll-preserve-screen-position nil)
  (setq auto-window-vscroll nil)
  (setq next-screen-context-lines 30)
  ;; Cleaner visuals, max. decoration.
  (setq line-spacing nil)
  (setq truncate-lines t)
  (setq font-lock-maximum-decoration t)
  (setq diff-font-lock-syntax t)
  (setq fringe-mode 1) ; This is the value for "minimal".
  (setq global-hl-line-mode t)

  ;; Clipboard behavior.
  (setq x-select-enable-clipboard-manager t)

  ;; Debugging.
  (setq debug-on-error t)
  (setq init-file-debug t)

  ;; Save-related settings.
  (setq save-place-mode t)
  (setq desktop-save-mode nil)
  (setq blink-cursor-mode t)

  ;; History.
  (setq history-length 1000)
  (setq history-delete-duplicates t)

  ;; Better interpreter settings: scroll down with input/output.
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)) ; Not sure what this does.

(prog1 "Warnings."
  (setq warning-suppress-types '((yasnippet backquote-change))))

(prog1 "File-related settings."
  (use-package no-littering
    :defer nil
    :config
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (setq no-littering-var-directory (expand-file-name "var/" user-emacs-directory))
    (setq no-littering-autosave-directory (expand-file-name "autosave/" no-littering-var-directory))
    (setq no-littering-backup-directory (expand-file-name "backup/" no-littering-var-directory))
    (setq no-littering-abbrev-directory (expand-file-name "abbrev/" no-littering-var-directory))

    (prog1 "recentf"
      (setq recentf-exclude '(no-littering-var-directory))
      (setq recentf-exclude '(no-littering-etc-directory)))

    ;; The following snippet checks if a file specified in my-custom-file exists.
    ;; If it does, set it as custom-file and load it.  If it does not, create the
    ;; file with "touch", set it as custom-file, and load it.
    (prog1 (message "%s"
                    (concat
                     "Looking for a customization file: "
                     custom-file))
      (when (not (file-exists-p custom-file))
        (progn
          (message "%s" "No customization file found, creating empty file.")
          (eshell-command
           (concat "touch " custom-file))
          (message "%s" "Created empty file.")))
      (if (file-exists-p custom-file)
          (progn
            (message "%s" "Customization file found.")
            (load custom-file))
        (message "%s" "ERROR: Cannot find customization file.")))


    (prog1 "auto-save-files"
      (setq auto-save-default t)
      (setq auto-save-timeout 15)
      (setq auto-save-interval 60)
      (setq auto-save-list-file-prefix no-littering-autosave-directory)
      (setq auto-save-file-name-transforms `((".*"
                                              ,no-littering-autosave-directory
                                              t))))

    (use-package abbrev
      :ensure nil
      :straight nil
      :diminish abbrev-mode
      :config
      (setq save-abbrevs 'silently)
      (setq abbrev-file-name no-littering-abbrev-directory)
      (if (file-exists-p abbrev-file-name)
	  (quietly-read-abbrev-file)))

    (prog1 "files"
      (setq require-final-newline t)
      (setq make-backup-files t)
      (setq backup-by-copying t)
      (setq kept-new-versions 2)
      (setq kept-old-versions 2)
      (setq version-control t)
      (setq delete-old-versions t)
      (setq backup-directory-alist `(("." . ,no-littering-backup-directory)
                                     (,tramp-file-name-regexp . nil))))

    (prog1 "*lock-files"
      (setq create-lockfiles nil))))

(prog1 "Line-numbering."
  (setq display-line-numbers nil) ; No line numbers (prog-mode only).
  (setq display-line-numbers-width 4) ; Default width.
  (setq display-line-numbers-widen t) ; Don't disregard narrowing.

  ;; Only enable line numbers in prog-mode.
  (progn
    (add-hook 'prog-mode-hook #'display-line-numbers-mode)
    (add-hook 'conf-mode-hook #'display-line-numbers-mode)
    (add-hook 'yaml-mode-hook #'display-line-numbers-mode)))

(prog1 "Misc. functions."

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

(use-package treesit-auto
  :disabled t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package undo-tree
  :disabled t
  :defer nil
  :after no-littering
  :diminish undo-tree-mode
  :bind
  (("C-c u t" . undo-tree-visualize))
  :config
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode t))

(prog1 "Mode-line-settings."
  ;; These options have to be included in mode-line-format as well.
  (column-number-mode 1) ; Show column number.
  (line-number-mode 1) ; Show line number in mode line.

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

(use-package auto-compile
  :defer nil
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :bind
  (("C-c y i" . yas-insert-snippet)
   ("C-c y v" . yas-visit-snippet-file))
  :config
  (add-hook 'python-base-mode-hook #'(lambda () (yas-activate-extra-mode 'python-mode)))
  (use-package yasnippet-snippets)
  (setq yas-indent-line 'fixed)
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 (list path-to-my-snippets)))
  (yas-reload-all)
  (yas-global-mode))

(prog1 "Dired setup."
  (prog1 "Dired."
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (setq dired-dwim-target t) ; Better target.
    (setq dired-recursive-copies 'always) ; Copy recursively.
    (setq dired-recursive-deletes 'always) ; Delete recursively.
    (setq dired-hide-details-hide-symlink-targets nil) ; Show symlinks.
    (setq dired-listing-switches "-lahgF --group-directories-first")
    (setq dired-kill-when-opening-new-dired-buffer nil)
    (setq delete-by-moving-to-trash t))

  (use-package dired-du
    :after dired
    :diminish dired-du-mode
    :config
    (setq dired-du-size-format t))

  (use-package dired-subtree
    :after dired
    :bind
    (:map
     dired-mode-map
     (";" . dired-subtree-toggle)
     ("'" . dired-subtree-remove))
    :config
    (setq dired-subtree-use-backgrounds nil)
    (setq dired-subtree-line-prefix "   |-")))

(prog1 "tramp"
  (setq tramp-debug-buffer t)
  (setq tramp-read-passwd t)
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 10)
  (use-package tramp-term
    :after tramp))

(use-package async
  :defer nil
  :diminish dired-async-mode
  :config
  (setq dired-async-mode 1)
  (setq async-bytecomp-package-mode 0)) ; Not sure if this creates issues.

(use-package flyspell
  :diminish flyspell-mode
  :hook
  ((prog-mode . (lambda() (flyspell-prog-mode)))
   (text-mode . (lambda() (flyspell-mode))))
  ;; Deactivate for logs and log editing.
  ;; (log-edit-mode-hook . (lambda() (flyspell-mode -1)))
  ;; (change-log-mode-hook . (lambda() (flyspell-mode -1))))
  :config
  ;; If Hunspell is present, setup Hunspell dictionaries.
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell") ; Use Hunspell.
          ispell-local-dictionary "en_US"
          ispell-dictionary "en_US"
          ispell-really-hunspell nil ; Temporary fix for Hunspell 1.7.
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

(prog1 "Helm setup"
  (use-package helm
    :defer nil
    :diminish (helm-mode helm-autoresize-mode helm-minibuffer-history-mode)
    :requires helm-autoloads
    :bind*
    (("M-x" . helm-M-x)
     ("C-s" . helm-occur)
     ("C-x b" . helm-mini)
     ("C-x C-f" . helm-find-files)
     ("M-y" . helm-show-kill-ring)
     ("C-c h" . helm-command-prefix)
     ("C-c t h" . helm-tramp)
     (:map
      helm-command-map
      ("l" . helm-locate)
      ("s" . helm-surfraw)
      ("r" . helm-regexp)
      ("m" . helm-multi-files)
      ("a" . helm-apropos)
      ("i" . helm-imenu)))
    :init
    ;; Remove old bind for helm-command-map.
    (global-unset-key
     (kbd "C-x c"))
    :config
    ;; Splitting behavior.
    (setq helm-split-window-inside-p nil)
    (setq helm-move-to-line-cycle-in-source nil) ; If t breaks cycling.
    (setq helm-autoresize-mode t)
    ;; Use fuzzy matching when possible.
    (setq helm-mode-fuzzy-match t)
    (setq helm-completion-in-region-fuzzy-match t)
    ;; (setq helm-display-function 'helm-display-buffer-in-own-frame)
    (setq helm-display-buffer-reuse-frame nil)
    (setq helm-use-undecorated-frame-option t)
    ;; Some helm-tramp settings.
    (setq helm-tramp-control-master t)
    ;; Turn on helm-mode.
    (helm-mode 1))

  (use-package helm-tramp
    :defer nil
    :after helm tramp)

  (use-package helm-ag
    :defer nil
    :after helm)

  (use-package helm-flyspell
    :defer nil
    :after helm flyspell
    :bind
    (("C-c f c" . helm-flyspell-correct))))

(use-package base16-theme
  :defer nil
  :config
  (load-theme 'base16-zenburn t)

  ;; Change the terminal colors.  Not sure if it works.
  (setq base16-theme-256-color-source "colors")

  ;; Replace the name of the theme if necessary.
  (prog1 "Create a variable for each color"
    (defvar base00-prop
      (nth 01 base16-zenburn-theme-colors))
    (defvar base01-prop
      (nth 03 base16-zenburn-theme-colors))
    (defvar base02-prop
      (nth 05 base16-zenburn-theme-colors))
    (defvar base03-prop
      (nth 07 base16-zenburn-theme-colors))
    (defvar base04-prop
      (nth 09 base16-zenburn-theme-colors))
    (defvar base05-prop
      (nth 11 base16-zenburn-theme-colors))
    (defvar base06-prop
      (nth 13 base16-zenburn-theme-colors))
    (defvar base07-prop ; White.
      (nth 15 base16-zenburn-theme-colors))
    (defvar base08-prop ; Pink.
      (nth 17 base16-zenburn-theme-colors))
    (defvar base09-prop ; Orange.
      (nth 19 base16-zenburn-theme-colors))
    (defvar base0A-prop ; Yellow.
      (nth 21 base16-zenburn-theme-colors))
    (defvar base0B-prop ; Green.
      (nth 23 base16-zenburn-theme-colors))
    (defvar base0C-prop
      (nth 25 base16-zenburn-theme-colors))
    (defvar base0D-prop
      (nth 27 base16-zenburn-theme-colors))
    (defvar base0E-prop
      (nth 29 base16-zenburn-theme-colors))
    (defvar base0F-prop
      (nth 31 base16-zenburn-theme-colors)))

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
   '(font-lock-keyword-face ((t (:weight bold))))
   '(font-lock-builtin-face ((t (:weight bold))))))

;; '(font-lock-function-name-face ((t (:weight bold))))
;; '(font-lock-comment-delimiter-face ((t (:slant italic))))
;; '(font-lock-comment-face ((t (:slant italic))))

(use-package avy
  :defer nil
  :after base16-theme
  :bind*
  (("C-z" . avy-goto-char))
  :init
  (global-unset-key (kbd "S-SPC")) ; Unbind scroll down from S-SPC.
  :config
  ;; (define-key global-map (kbd "M-S-SPC") nil)
  ;; (global-set-key (kbd "M-S-SPC") 'avy-goto-char)
  (setq avy-background t)
  (setq avy-all-windows t)
  ;; NOT SURE IF THIS IS CORRECT: When non-nil highlight the first decision
  ;; char with avy-lead-face-0.  Do this even when the char is terminating.
  ;; Normally avy-lead-face-0 is only used for the first non-terminating
  ;; decision chars.
  (setq avy-highlight-first t)

  ;; Using any command makes the face attributes accessible.
  (avy-setup-default)

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
                      :foreground base00-prop
                      :background base0E-prop
                      :weight 'bold)

  ;; Face for foreground/font during selection: base03.
  (set-face-attribute 'avy-background-face nil
                      :foreground base03-prop
                      :background base00-prop))

;; Provides a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1)
  (setq which-key-idle-secondary-delay 0)
  (which-key-mode 1))

;; Sources: https://github.com/rejeep/emacs/blob/master/init.el
(prog1 "parens"
  (setq show-paren-delay 0.0)
  (setq show-paren-mode t)
  ;; From
  ;; https://github.com/conao3/dotfiles/commit/d9c0f0dc55e7c65517b2c9ce8eb01f96a425ffd1#diff-f48385f05c9a82908d8bd23c391bbbd3
  (use-package smartparens
    :defer nil
    :diminish (smartparens-mode smartparens-global-mode)
    :bind*
    (("C-c u s" . sp-unwrap-sexp))
    :config
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (setq sp-highlight-pair-overlay nil)
    (setq sp-show-pair-from-inside t)))

(use-package highlight-indent-guides
  ;; From https://github.com/DarthFennec/highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :hook
  ((python-base-mode . highlight-indent-guides-mode)
   (yaml-ts-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0))

(prog1 "company-setup"
  (use-package company
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
    (add-to-list 'company-backends 'company-math-symbols-unicode)))

(use-package aggressive-indent
  ;; From https://github.com/Malabarba/aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Make sure that there is a single additional line at the end of the file
;; while saving, also removes all white space at the end of lines.
(use-package whitespace
  :after base16-theme
  :diminish whitespace-mode
  :init
  (setq delete-trailing-lines t)
  ;; (setq show-trailing-whitespace t)
  :hook
  ((prog-mode . whitespace-mode)
   (markdown-mode . whitespace-mode))
  :config
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq-local whitespace-mode 0)))
  ;; Set the max. column as defined above and delete trailing lines.
  (setq whitespace-line-column my-max-columns)
  (setq whitespace-style '(face
                           ;; tabs
                           ;; spaces
                           trailing
                           ;; lines
                           lines-tail
                           ;; lines-char
                           ;; newline
                           empty
                           space-before-tab
                           space-after-tab
                           ;; indentation
                           ;; space-mark
                           tab-mark
                           ;; newline-mark
                           missing-newline-at-eof)))

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (add-to-list 'ws-butler-global-exempt-modes '(magit-mode))
  (ws-butler-global-mode t))

;; Basic bindings for multiple-cursors.
(use-package multiple-cursors
  :defer nil
  :bind*
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c C->" . mc/mark-all-like-this)))

(use-package flycheck
  :diminish (global-flycheck-mode flycheck-mode)
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error))
  :hook
  (after-init . global-flycheck-mode))

(prog1 "*python-setup"
  ;; Activate tree-sitter for Python.
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  (setq python-indent-offset 4)

  (use-package conda
    :disabled t
    :after dired
    :bind
    (("C-c $" . conda-env-activate))
    :config
    (setq conda-anaconda-home "~/miniconda3/")
    (setq conda-env-executables-dir "condabin")
    ;; Interactive shell support, include.
    (conda-env-initialize-interactive-shells)
    ;; Eshell support.
    (conda-env-initialize-eshell))

  (use-package flycheck-pycheckers
    :after (flycheck python)
    :config
    (setq flycheck-pycheckers-multi-thread "true")
    (setq flycheck-pycheckers-max-line-length 88) ; Follow Black guidelines.
    (setq flycheck-pycheckers-checkers '(python-ruff pylint mypy3))

    ;; TODO: Add this to :hook.
    (with-eval-after-load
        'flycheck (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

  (use-package reformatter
    :config
    (use-package ruff-format
      ;; Usage:
      ;; ((python-base-mode . ((eval . (progn
      ;;                                 (ruff-format-on-save-mode)
      ;;                                 (flymake-ruff-load)
      ;;                                 )
      ;;                             ))
      ;;                    )
      ;; (nil . ((eval . (add-hook 'after-save-hook 'push-ior-via-kubectl))))
      ))


  (use-package flymake-ruff
    :disabled t
    :hook (python-base-mode . flymake-ruff-load))

  (use-package sphinx-doc
    :disabled t
    :load-path "~/gitdir/my-git/sphinx-doc.el/"
    :diminish sphinx-doc-mode
    :hook
    (python-base-mode . sphinx-doc-mode)
    :config
    ;; Show all arguments (except "self").
    (setq sphinx-doc-all-arguments t)
    (setq sphinx-doc-exclude-rtype t))

  (use-package python-isort)

  (use-package pyimport
    :bind
    (:map
     python-mode-map
     ("C-c m i" . pyimport-insert-missing)
     ("C-c u r" . pyimport-remove-unused)))

  (use-package python-docstring
    :hook
    (python-base-mode . python-docstring-mode)))

(prog1 "Setting up Rust..."
  (use-package rust-mode
    :config
    (add-hook 'rust-mode-hook
              (lambda () (setq indent-tabs-mode nil)))
    (setq rust-format-on-save t)
    (add-hook 'rust-mode-hook
              (lambda () (prettify-symbols-mode)))
    ;; Activate tree-sitter for Rust.
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))))

(use-package org ; FIXME: Band aid > Use :bind at some point.
  :straight
  (
   :package org
   :type git
   :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
   :local-repo "org"
   :depth 1
   )
  :mode "//.org$"
  :bind*
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  ;; ("C-c l" . org-store-link) ; Store link.
  (:map
   org-mode-map (("C-c o c i" . org-clock-in)
                 ("C-c o c o" . org-clock-out)
                 ("C-c o s n" . org-toogle-narrow-to-subtree)
                 ("C-c o i" . org-id-get-create)))
  :hook
  ;; Align tags when saving.
  (org-mode-hook . (lambda ()
                     (add-hook 'before-save-hook 'org-align-tags nil 'local)))
  ;; Switch to DONE when sub-entries are done.
  (org-after-todo-statistics-hook . org-summary-todo)
  ;; ;; Highlight current line in agenda.
  (org-agenda-mode-hook . (lambda () (hl-line-mode 1)))
  :config
  (prog1 "Setting directories without :custom"
    (setq org-directory my-notes-dir)
    (setq org-default-notes-file my-notes)
    (setq org-todo-file my-todos)
    (setq org-agenda-files (list org-directory my-roam-notes)))
  (let ((work-notes "~/gitdir/my-git/my-work-dirs/notes.el"))
    (if (file-exists-p work-notes)
        (progn
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

  ;; Misc.
  (setq org-src-window-setup 'other-window)
  (setq org-tags-column 70)
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
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-include-deadlines t)
  (setq org-agenda-include-diary nil)
  ;; (setq org-agenda-block-separator nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-start-with-log-mode nil)
  ;; Better calendar settings: Include last week only if today is Monday,
  ;; always show three weeks. and always start the week on Monday.
  (setq calendar-week-start-day 1)
  ;; (setq org-agenda-start-day . "-7d")
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-span 9)

  ;; Style-related.
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (defun org-syntax-convert-keyword-case-to-lower ()
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

  (defun modi/lower-case-org-keywords ()
    "Lower case Org keywords and block identifiers.

             Example: \"#+TITLE\" -> \"#+title\"
                      \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"

             Inspiration:
             https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (count 0))
        ;; Match examples: "#+FOO bar", "#+FOO:", "=#+FOO=", "~#+FOO~",
        ;;                 "‘#+FOO’", "“#+FOO”", ",#+FOO bar",
        ;;                 "#+FOO_bar<eol>", "#+FOO<eol>".
        (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
          (setq count (1+ count))
          (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
        (message "Lower-cased %d matches" count))))


  ;; Always insert blank line before headings.
  (setq org-blank-before-new-entry '((heading . auto)
                                     (plain-list-item . auto)))

  (prog1 "*org-refile"
    (setq org-refile-use-outline-path 'full-file-path)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9))))

  (prog1 "*org-capture-templates"
    ;; Templates for org-capture
    ;; If the directory exists, load my templates.
    (let ((templates my-org-templates))
      (if (file-exists-p templates)
          (progn
            (message "%s" "Adding templates...")
            (load templates))
        (message "%s" "No templates specified.")))

    ;; If the directory exists, load templates for work.
    (let ((templates "~/gitdir/my-git/my-work-emacs-init/templates.el"))
      (if (and (file-exists-p templates)
               (boundp 'org-capture-templates))
          (progn
            (message "%s" "Adding templates for work...")
            (load templates))
        (message "%s" "No work-related templates specified."))))

  (prog1 "*org-summary-todo"
    ;; Switch entry to DONE when all subentries are done, to TODO
    ;; otherwise.
    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done-with-time
            org-log-states) ; turn off logging
        (org-todo
         (if (= n-not-done 0)
             "DONE" "TODO")))))

  ;; Don't confirm before evaluating.
  (setq org-confirm-babel-evaluate nil)
  ;; Available languages: https://orgmode.org/org.html#Languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (org . t)
     (python . t)
     (R . t)
     (latex . t)))
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

  (use-package org-super-agenda
    :after org
    :config
    (setq org-agenda-include-deadlines t)
    (setq org-agenda-block-separator 61)
    (setq org-agenda-compact-blocks nil)
    ;; org-agenda-start-with-log-mode t
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
                                    :not (:tag ("bill"
                                                "shoppinglist"
                                                "reading"
                                                "datascience"
                                                "@work")))
                         :order 3)
                  (:name "Data science"
                         :and (:tag "datascience"
                                    :not (:tag "reading"))
                         :order 4)
                  (:name "Data science readings"
                         :and (:tag ("datascience" "towardsdatascience")
                                    :tag "reading")
                         :order 5)
                  (:name "Readings"
                         :category "readings"
                         :order 6)
                  (:name "Shopping list"
                         :tag "shoppinglist"
                         :order 7))))))))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t))

(use-package org-download
  :disabled t
  :after org
  :bind
  (:map
   org-mode-map
   (("C-c i s" . org-download-screenshot)
    ("C-c i y" . org-download-yank))))

(use-package org-roam
  :bind*
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n t c" . org-roam-dailies-capture-today)
   ("C-c n t g" . org-roam-dailies-goto-today)
   ("C-c n g" . org-roam-graph)
   ("C-c n a r" . org-roam-ref-add)
   ("C-c n a t" . org-roam-tag-add))
  :config
  (setq org-roam-directory my-roam-notes)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  (org-roam-db-autosync-mode))

(use-package deft
  :after org
  :bind*
  (("C-c n d" . deft))
  :config
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-directory my-roam-notes))

(use-package doc-view
  :disabled t
  :config
  (use-package pdf-tools
    :bind
    (:map
     pdf-view-mode-map
     ("C-s" . isearch-forward))
    :init
    (pdf-loader-install) ; Prepare Emacs for using PDF Tools.
    :config
    (setq pdf-view-display-size 'fit-page)
    (setq pdf-annot-activate-created-annotations t)
    :config
    (use-package org-pdfview
      :config
      (add-to-list 'org-file-apps
                   '("\\.pdf\\'" .
                     (lambda (file link)
                       (org-pdfview-open link)))))))

(use-package nov
  :disabled t
  ;; Reading .epub files.
  :straight (nov :type git :repo "https://depp.brause.cc/nov.el/")
  :config
  (add-to-list 'auto-mode-alist
               '("\\.epub\\'" . nov-mode)))

(prog1 "*git-tools"
  :config
  ;; Fixes temporary issues with vc-mode.
  (setq vc-handled-backends ())

  (use-package hl-todo
    :config
    (global-hl-todo-mode t))

  (prog1 "ediff"
    (setq ediff-window-setup-function 'ediff-setup-windows-plain) ; Don't start another frame.
    ;; Revert windows on exit - needs winner mode
    (winner-mode)
    (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

  (use-package magit
    :diminish magit-auto-revert-mode
    :bind*
    (("C-x g" . magit-status))
    :config
    (magit-auto-revert-mode t)
    (setq magit-diff-refine-hunk 'all)
    (setq magit-log-auto-more t)
    (prog1 "autorevert"
      (setq auto-revert-interval 1)
      (setq global-auto-revert-mode nil)))

  (use-package git-timemachine
    ;; https://codeberg.org/pidu/git-timemachine
    :diminish git-timemachine-mode
    :bind (("C-c t m" . git-timemachine)))

  (use-package git-auto-commit-mode
    :diminish git-auto-commit-mode
    :config
    (setq gac-automatically-push-p t)))

;; Emacs Refactor (EMR) is a framework for providing language-specific
;; refactoring in Emacs.
(use-package emr
  :disabled t
  :config
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu))

(use-package eglot
  :disabled t
  :hook
  (python-base-mode . eglot-ensure)
  :bind
  (:map
   eglot-mode-map
   ("C-c e r" . eglot-rename)
   ("C-c e c a" . eglot-code-actions)
   ("C-c d o c" . eldoc)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((python-base-mode . lsp-deferred)
   (rust-ts-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind
  (:map
   lsp-mode-map
   ("M-?" . lsp-find-references)
   ("M-." . lsp-find-definition))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-diagnostics-provider :none)
  :config
  (setq lsp-file-watch-threshold 10000)
  (setq lsp-restart 'auto-restart)
  (lsp-register-custom-settings
   '(
     ("pylsp.plugins.pylsp_mypy.enabled" nil nil)
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
     ("pylsp.plugins.flake8.enabled" nil nil)
     ))
  (use-package helm-lsp
    :after (helm lsp)
    :commands helm-lsp-workspace-symbol)
  (use-package lsp-pyright
    :after (python lsp)
    :disabled t
    :hook
    (python-base-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

;; Always use GPG2 and use loopback option for better compatibility.
(use-package epa
  :config
  (setq epa-pinentry-mode 'loopback)
  (prog1 "epa-config"
    (setq epg-gpg-program "gpg2"))
  (prog1 "auth-source"
    (setq auth-sources '("~/.authinfo.gpg")))
  (use-package pinentry))

(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :config
  (define-key yaml-ts-mode-map (kbd "C-m") 'newline-and-indent))

(use-package csv-mode
  :disabled t
  :config
  (add-to-list 'auto-mode-alist
               '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t))

(use-package ssh-deploy
  ;; Effortlessly deploy local files and directories to remote
  ;; hosts via Tramp:
  ;; https://github.com/cjohansson/emacs-ssh-deploy
  :disabled t
  :bind
  (("C-c z d" . ssh-deploy-prefix-map))
  :hook
  ((after-save-hook . ssh-deploy-after-save)
   (find-file . ssh-deploy-find-file))
  :config
  (setq ange-ftp-netrc-filename "~/.authinfo.gpg")
  (ssh-deploy-line-mode)
  (ssh-deploy-add-menu))

(use-package projectile
  :diminish projectile-mode
  :bind
  (("C-c p" . projectile-command-map))
  :init
  (projectile-mode t)
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  (add-to-list 'projectile-globally-ignored-files '"*ediff-merge*"))

(use-package helm-projectile
  :after (projectile helm)
  :config
  (helm-projectile-on))

(use-package json-mode
  :config
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package kubernetes
  :bind
  (("C-c k o" . kubernetes-overview))
  :commands (kubernetes-overview))

;;; From https://www.emacswiki.org/emacs/UnfillParagraph.
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(defun copy-git-current-sha ()
  "Copy the current Git commit SHA to the clipboard."
  (interactive)
  (let ((sha (string-trim (shell-command-to-string "git rev-parse HEAD"))))
    (when (string-match-p "^[0-9a-f]\\{40\\}$" sha)
      (kill-new sha)
      (message "Copied SHA: %s" sha))))
(define-key global-map (kbd "C-c c s") 'copy-git-current-sha)

(defun copy-current-path-to-file ()
  "Copies the path of the current file to the clipboard."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "Copied file path: %s" buffer-file-name))
    (message "No file is currently visiting.")))
(define-key global-map (kbd "C-c c p") 'copy-current-path-to-file)

;;; Footer:
(provide 'init)
;;; init.el ends here
