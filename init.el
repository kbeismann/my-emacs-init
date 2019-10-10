;;; init.el --- Emacs initialization file -*- lexical-binding: nil; coding: utf-8 -*-


;; Copyright (C) 2019 Karsten E. Beismann

;; Author: Karsten Beismann
;; Homepage: https://github.com/kbeismann/emacs-init
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

;; Some optional settings, e.g. my mu4e setup, are not part of this
;; repository.


;;; Installation:

;; * Install fonts if necessary.
;; * Create a symbolic link or copy this file to ~/.emacs.


;;; Working font options:

;; * Hack:
;; ** "Hack-9" ; Arch
;; ** "Hack:pixelsize=14" ; Arch

;; * DejaVu Sans Mono:
;; ** "DejaVu Sans Mono-10"; Arch

;; * Inconsolata:
;; ** "Inconsolata-11"
;; ** "Inconsolata:pixelsize=14" ; Arch

;; * Dina:
;; ** "Dina-9"
;; ** "Dina:pixelsize=12" ; Arch, Manjaro
;; ** "-*-dina-medium-r-*-*-12-*-*-*-*-*-*-*"

;; * Terminus:
;; ** "Terminus-12":
;; ** "xos4 Terminus-10" ; Arch, Manjaro
;; ** "-*-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*"
;; ** "-xos4-terminus-medium-r-normal-*-14-120-*-*-*-*-*-*" ; Ubuntu
;; ** "-xos4-terminus-medium-r-normal--16.5-120-*-*-*-*-*-*" ; Ubuntu


;;; Sources (incomplete):

;; * https://github.com/rememberYou/.emacs.d/
;; * https://github.com/conao3/dotfiles/blob/master/.dotfiles/.emacs.d/init.el
;; * https://gitlab.com/k-bps/mesk/blob/master/src/init.org
;; * https://github.com/zamansky/using-emacs


;;; To-do:

;; * TODO: Init file in org?
;; * TODO: Better YASnippet implementation.
;; * TODO: Use function/package to toggle proxies.


;;; Code:


;;; STARTUP TIME


;; This hook returns the loading time after startup.  A hook is used so the
;; message does not get clobbered with other messages.

(prog1 "Show startup time"

  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done))))


;;; LESS GC DURING STARTUP


;; This block effectively disables garbage collection for the initialization
;; time and re-enables it after.  If the machine has enough RAM, at most 64MB
;; every time you start up a new Emacs, this will reduce package-initialize
;; time to about half.

(prog1 "Improve startup time"

  ;; (setq gc-cons-threshold 64000000) ; Former value.

  ;; Before startup, increase threshold.
  (setq gc-cons-threshold most-positive-fixnum)

  ;; Restore consing between collection after initialization.
  (add-hook 'after-init-hook #'(lambda ()
                                 (setq gc-cons-threshold 800000)))

  ;; Let's increase the max-lisp-eval-depth and max-specpdl-size to
  ;; prevent exceeding recursion limits.
  (setq max-lisp-eval-depth 50000
        max-specpdl-size 10000)

  ;; Disable certain byte compiler warnings to cut down on the noise.
  (setq byte-compile-warnings '(not free-vars unresolved noruntime
                                    lexical make-local)))


;;; LEAF SETUP


(prog1 "Use leaf to simplify package management"

  ;; Add archives and assign priorities.
  ;; (setq package-check-signature nil)    ; Do/don't check sig. ; TODO: Try without and remove if possibles.
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/"))
        package-archive-priorities '(("gnu" . 2)
                                     ("org" . 1)
                                     ("melpa" . 3)
                                     ("melpa-stable" . 0)))

  ;; Initialize package BEFORE installing/loading leaf.
  (package-initialize)

  ;; Install leaf if necessary.
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords

    :ensure t

    :config

    (leaf el-get

      :ensure t

      :init

      (unless (executable-find "git")
	(warn "Git not found: el-get can't download packages."))

      :custom

      ((el-get-git-shallow-clone . t)
       (el-get-emacswiki-base-url . "http://www.emacswiki.org/emacs/download/")))

    (leaf diminish

      :ensure t)

    (leaf-keywords-init)
    (message "Leaf initiated with additional keywords...")))


;; WORK-RELATED SETTINGS


(leaf *work-related-settings

  :doc "Load work-related settings if file exists"

  :config

  (let ((proxies "~/gitdir/emacs-work/proxies.el"))
    (when (file-exists-p proxies)
      (load proxies))))


;; BASIC VARIABLES


(leaf *basic-variables

  :doc "Basic variables"

  :config

  (defvar my-autosave-dir
    (concat user-emacs-directory "autosave/")
    "My auto-save-list directory.")

  (defvar my-backup-dir
    (concat user-emacs-directory "backup/")
    "My backup directory.")

  (defvar my-cache-dir
    (concat user-emacs-directory "cache/")
    "My storage area(cache) directory.")

  (defvar my-font "Dina:pixelsize=12"	; TODO: Currently not in use.
    "My default font.")

  (defvar my-max-columns 78
    "My predefined characters per line (CPL) limit."))


;;; BASIC SETTINGS

(leaf *basic-settings

  :bind

  (("M-o". nil)                          ; Unbind face menu.
   ("C-x C-z" . nil)                     ; Unbind suspend frame.
   ("S-SPC" . 'just-one-space))           ; Bind just-one-space.


  :custom

  ((user-full-name . "Karsten Beismann")

   ;; Misc. settings.
   (ring-bell-function . 'ignore)             ; No annoying bell.
   (inhibit-startup-screen . t)		      ; No starting screen.
   (mouse-yank-at-point . t)		      ; Paste at cursor, not at mouse.
   (vc-follow-symlinks . t)		      ; Always follow symbolic links.
   (indent-tabs-mode . nil)		      ; Always indent with spaces.
   (tab-width . 4)			      ; Default tab width.
   (next-line-add-newlines . t)		      ; New line when C-n.
   (fill-column . my-max-columns)	      ; Set M-q columns.
   (large-file-warning-threshold . 100000000) ; Large file warning.
   (tab-always-indent 'complete)	      ; Tab indents before completion.

   ;; Better scrolling behavior.
   (scroll-margin . 0)
   (scroll-conservatively . 10000)
   (scroll-preserve-screen-position . nil)
   (auto-window-vscroll . nil)

   ;; Cleaner visuals.
   (menu-bar-mode    . nil)
   (tool-bar-mode    . nil)
   (scroll-bar-mode . nil)
   (truncate-lines . t)
   (line-spacing . nil)
   (font-lock-maximum-decoration . t)

   ;; Clipboard behavior.
   (x-select-enable-clipboard-manager . t)

   ;; Debugging.
   (debug-on-error . t)
   (init-file-debug . t ))

  :config

  (leaf saveplace

    :doc "Point goes to the last place where it was."

    :custom

    ((save-place-mode . t)))

  (leaf desktop

    :doc "The state of Emacs is saved from one session to another."

    :custom

    ((desktop-save-mode . nil)))

  (leaf frame

    :custom

    ((blink-cursor-mode . t)))

  (leaf cus-edit

    :doc "Prevent customization information from littering init.el."

    :custom

    ((custom-file . "~/.custom.el"))

    :config

    (load custom-file))

  (leaf autorevert

    :custom

    :custom

    ((auto-revert-interval . 5)
     (global-auto-revert-mode . t)))

  (fset 'yes-or-no-p 'y-or-n-p))                  ; y/p instead of yes/no.



;;; FONT AND FRAME SETTINGS


;; Font and frame settings, dependent on the OS.

;; TODO: Check if font is present before setting it.

(leaf *os-related-settings

  :doc "Check OS and set appropriate font"

  :config

  (message "%s" "Checking OS...")
  (when (eq system-type 'gnu/linux)

    ;; Get OS designation without quotes.
    (defvar my-os (substring (shell-command-to-string "lsb_release -sd") 1 -2))
    (message "%s" "Found GNU/Linux...")
    (message "Found distribution: %s" my-os)

    ;; Font for Manjaro.
    (if (string-equal "Manjaro"
		      (substring my-os 0 7))
	(progn (add-to-list 'default-frame-alist '(font . "Dina:pixelsize=12"))
	       ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
	       (add-to-list 'default-frame-alist '(height . 50))
	       (add-to-list 'default-frame-alist '(width . 180)))

      ;; (add-to-list 'default-frame-alist '(font . "xos4 Terminus:pixelsize=14"))
      (when (string-equal
	     (substring (shell-command-to-string "lsb_release -sd") 0 3)
	     (substring "Ubun" 0 3)) ; TODO: Adjust if necessary.
	(progn (add-to-list 'default-frame-alist
			    '(font . "-xos4-terminus-medium-r-normal--16.5-120-*-*-*-*-*-*"))
	       (add-to-list 'default-frame-alist '(height . 50))
	       (add-to-list 'default-frame-alist '(width . 200)))))))


;;; BACKUPS/ABBREVS/LOCKFILES/CUSTOMIZE


(leaf *backup-settings

  :doc "Backups and more"

  :config


  ;; Abbrev options.
  (setq save-abbrevs nil                  ; Save abbrevs when files are saved.
	save-abbrevs 'silently            ; Silent abbrevs.
	abbrev-file-name "~/.emacs.d/abbrev_defs" ) ; Abbrevs.

  ;; Disable lock-files.
  (setq create-lockfiles nil)

  ;; Backup options.
  (setq make-backup-files nil
	backup-by-copying t               ; Don't clobber symlinks.
	delete-old-versions t             ; Remove old versions.
	kept-new-versions 2               ; Number of newest versions to keep.
	kept-old-versions 2               ; Number of oldest versions to keep.
	version-control t                 ; Use versioned backups.
	backup-directory-alist
	'(("." . "~/emacsauto/backups"))) ; Don't litter my fs tree.

  ;; Autosave options.
  (setq auto-save-default t               ; #autosave# files.
	auto-save-interval 300            ; Autosave every 500 characters.
	auto-save-list-file-prefix "~/emacsauto/autosaves"
	auto-save-file-name-transforms
	'((".*" "~/emacsauto/autosaves" t))))


;;; LINE NUMBERING



(leaf *line-numbering

  :doc "The display-line-numbers colors can be changed by editing base16.el."

  :custom

  ((display-line-numbers . nil)		; No line numbers (prog-mode only).
   (display-line-numbers-width . 4)	; Default width.
   (display-line-numbers-widen . t))	; Don't disregard narrowing.

  :config

  ;; Only enable line numbers in prog-mode.
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))



;;; MISC. FUNCTIONS


(leaf *misc-functions

  :config

  ;; Using the shell to insert the date.
  (defun insert-current-date ()
    "Insert the current date and time in a standard Emacs format."
    (interactive)
    (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))
  (global-set-key (kbd "C-c i d") 'insert-current-date)


  ;; Find non ASCII characters.
  (defun find-first-non-ascii-char ()
    "Find the first non-ASCII character from point onward."
    (interactive)
    (let (point)
      (save-excursion
	(setq point
	      (catch 'non-ascii
		(while (not (eobp))
		  (or (eq (char-charset (following-char))
			  'ascii)
		      (throw 'non-ascii (point)))
		  (forward-char 1)))))
      (if point
	  (goto-char point)
	(message "No non-ASCII characters."))))
  (global-set-key (kbd "C-S-s") 'find-first-non-ascii-char))


;;; MODE LINE


(leaf *mode-line-settings

  :config

  ;; These options have to be included in mode-line-format as well.
  (column-number-mode 1)                  ; Show column number.
  (line-number-mode 1)                    ; Show line number in mode line.


;;; SIMPLIFY THE CURSOR POSITION

  ;; Source:

  ;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/

  ;; No proportional position (percentage) nor texts like "Bot", "Top" or "All".

  (setq mode-line-position
	'(;; %p print percent of buffer above top of window, o Top, Bot or All.
	  ;; (-3 "%p")
	  ;; %I print the size of the buffer, with kmG etc.
	  ;; (size-indication-mode ("/" (-4 "%I")))
	  ;; " "
	  ;; %l print the current line number.
	  ;; %c print the current column.
	  (line-number-mode ("%l" (column-number-mode ":%c"))))))


;;; AUTO-COMPILE


(leaf auto-compile

  :ensure t

  :preface

  (setq load-prefer-newer t)

  :config

  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))


;;; AUTH-SOURCE


(leaf auth-source

  :ensure t

  :config

  (setq auth-sources '("~/.authinfo.gpg")))


;;; DIRED


(leaf dired

  :commands dired

  :ensure async

  :hook (dired-mode-hook . (lambda() (hl-line-mode 1)))

  :custom ((dired-dwim-target . t)	; Better default target directory.
	   (dired-recursive-copies . 'always)  ; Copy recursively.
	   (dired-recursive-deletes . 'always) ; Delete recursively.
	   (dired-hide-details-hide-symlink-targets . nil) ; Show symlinks.
	   (dired-listing-switches . "-lahgF --group-directories-first")))


;;; ASYNC


(leaf async

  :ensure t

  :config

  (dired-async-mode 1)
  (async-bytecomp-package-mode 0)) 	; Not sure if this creates issues.


;;; ESHELL


;; Check out https://github.com/jcf/emacs.d/blob/master/init-packages.org.

(leaf eshell

  :commands eshell

  :bind ("C-z" . eshell)

  :hook (eshell-mode-hook . my-eshell-remove-pcomplete)

  :config

  ;; Fixes weird issues in eshell.
  (defun my-eshell-remove-pcomplete ()
    (remove-hook 'completion-at-point-functions
                 #'pcomplete-completions-at-point t)))



;;; BIBTEX


(leaf bibtex

  :custom ((bibtex-autokey-additional-names . "_etal")
	   (bibtex-autokey-name-separator . "_")
	   (bibtex-autokey-names . 1)
	   (bibtex-autokey-names-stretch . 1)
	   (bibtex-autokey-name-length . 999)
	   (bibtex-autokey-name-year-separator . "-")
	   (bibtex-autokey-year-length . 4)
	   (bibtex-autokey-year-title-separator . "-")
	   (bibtex-autokey-titleword-separator . "_")
	   (bibtex-autokey-titlewords . 3)
	   (bibtex-autokey-titlewords-stretch . 1)
	   (bibtex-autokey-titleword-length . 5))


  :config

  ;; Path to library only set when directory exists.
  (let ((path-to-library "~/gitdir/library/"))
    (when (file-exists-p path-to-library)
      (setq bibtex-completion-library-path path-to-library)))

  (let ((path-to-bib "~/gitdir/library/bibliography.bib"))
    (when (file-exists-p path-to-bib)
      (setq bibtex-completion-bibliography path-to-bib))))




;;; DEBUGGING INIT FILE


(leaf bug-hunter

  :ensure t)


;;; DICTIONARY, FLYCHECK, AND FLYSPELL


(leaf flyspell

  :ensure t

  :diminish flyspell-mode

  :hook ((prog-mode-hook . (lambda() (flyspell-prog-mode)))
         (text-mode-hook . (lambda() (flyspell-mode))))

  ;; Deactivate for logs and log editing.
  ;; (log-edit-mode-hook . (lambda() (flyspell-mode -1)))
  ;; (change-log-mode-hook . (lambda() (flyspell-mode -1))))

  :config

  ;; HUNSPELL IS NOT USED ON MANJARO RIGHT NOW! REQUIRES ASPELL!

  ;; If Hunspell is present, setup Hunspell dictionaries.
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell") ; Use Hunspell.
          ispell-local-dictionary "en_US"
          ispell-dictionary "en_US"
          ispell-really-hunspell nil    ; Temporary fix for Hunspell 1.7.
          ispell-hunspell-dictionary-alist nil)

    ;; Settings for English, US.
    (add-to-list 'ispell-local-dictionary-alist '("english-hunspell"
                                                  "[[:alpha:]]"
                                                  "[^[:alpha:]]"
                                                  "[']"
                                                  t
                                                  ("-d" "en_US")
                                                  nil
                                                  iso-8859-1))

    ;; Settings for German, Germany.
    (add-to-list 'ispell-local-dictionary-alist '("deutsch-hunspell"
                                                  "[[:alpha:]]"
                                                  "[^[:alpha:]]"
                                                  "[']"
                                                  t
                                                  ("-d" "de_DE")
                                                  nil
                                                  iso-8859-1))))


;;; HELM


(leaf helm

  :ensure t

  :leaf-defer nil

  :require helm-config

  :diminish helm-mode

  ;; :ensure helm-R

  :bind (("M-x" . helm-M-x)
         ("C-s" . helm-occur)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-c h" . helm-command-prefix)
         (helm-command-map
          ("l" . helm-locate)
          ("s" . helm-surfraw)
          ("r" . helm-regexp)
          ("m" . helm-multi-files)
          ("a" . helm-apropos)))

  :init

  ;; Remove old bind for helm-command-map.
  (global-unset-key (kbd "C-x c"))

  :config

  ;; Splitting behavior.
  (setq helm-split-window-inside-p nil
        helm-move-to-line-cycle-in-source nil ; If t breaks cycling.
        helm-autoresize-mode t)

  ;; Use fuzzy matching when possible.
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t)

  ;; Turn on helm-mode.
  (helm-mode 1))


;; Use helm for Flyspell.
(leaf helm-flyspell

  :ensure t

  :after helm flyspell

  :bind ("C-c f c" . helm-flyspell-correct))


;; Use helm for BibTeX.
(leaf helm-bibtex

  :disabled t

  :ensure t

  :after helm bibtex

  :bind (helm-command-map
         ("b". helm-bibtex)))


;;; SET COLOR THEME


(leaf base16-theme

  :require t

  :config

  ;; Change the terminal colors.  Not sure if it works.
  (setq base16-theme-256-color-source "colors")

  ;; Load base16.
  (load-theme 'base16-zenburn 1)

  ;; Replace the name of the theme if necessary.
  (progn
    (defvar base00-prop (nth 01 base16-zenburn-colors))
    (defvar base01-prop (nth 03 base16-zenburn-colors))
    (defvar base02-prop (nth 05 base16-zenburn-colors))
    (defvar base03-prop (nth 07 base16-zenburn-colors))
    (defvar base04-prop (nth 09 base16-zenburn-colors))
    (defvar base05-prop (nth 11 base16-zenburn-colors))
    (defvar base06-prop (nth 13 base16-zenburn-colors))
    (defvar base07-prop (nth 15 base16-zenburn-colors))
    (defvar base08-prop (nth 17 base16-zenburn-colors))
    (defvar base09-prop (nth 19 base16-zenburn-colors))
    (defvar base0A-prop (nth 21 base16-zenburn-colors))
    (defvar base0B-prop (nth 23 base16-zenburn-colors))
    (defvar base0C-prop (nth 25 base16-zenburn-colors))
    (defvar base0D-prop (nth 27 base16-zenburn-colors))
    (defvar base0E-prop (nth 29 base16-zenburn-colors))
    (defvar base0F-prop (nth 31 base16-zenburn-colors)))

  ;; Remove the vertical line between windows:
  (set-face-background 'vertical-border base00-prop)
  (set-face-foreground 'vertical-border (face-background 'vertical-border))

  ;; Hide the fringe but show linebreak arrows.
  (set-face-attribute 'fringe
                      nil :background base00-prop :foreground base02-prop)

  ;; Look of the current line number.  Here, the background is the color
  ;; of the number.
  (set-face-attribute 'line-number-current-line
                      nil :background base08-prop :foreground base00-prop)

  ;; Look and color of the line numbers.
  (set-face-attribute 'line-number
                      nil :background base00-prop :foreground base02-prop)


  (custom-set-faces '(font-lock-keyword-face ((t (:weight bold))))
                    '(font-lock-builtin-face ((t (:weight bold)))))
  ;; '(font-lock-function-name-face ((t (:weight bold))))
  ;; '(font-lock-comment-delimiter-face ((t (:slant italic))))
  ;; '(font-lock-comment-face ((t (:slant italic))))
  )


;;; AVY


;; Move with the power of your mind and jump to things in Emacs tree-style.

(leaf avy

  :ensure t

  :after base16-theme

  :bind (( "M-SPC"   . avy-goto-char)
         ( "M-S-SPC" . avy-goto-char-2))

  :config

  (setq avy-background t
        avy-all-windows t
        avy-highlight-first t)          ; When non-nil highlight the first
                                        ; decision char with avy-lead-face-0.
                                        ; Do this even when the char is
                                        ; terminating.  Normally
                                        ; avy-lead-face-0 is only used for the
                                        ; first non-terminating decision
                                        ; chars.

  ;; Define colors for avy:

  ;; Face used for first non-terminating leading chars.
  (set-face-attribute 'avy-lead-face-0 nil
                      :foreground base0E-prop :background base00-prop
                      :weight 'bold)

  ;; Face used for matched leading chars.  Not sure what this does.
  (set-face-attribute 'avy-lead-face-1 nil :foreground base09-prop
                      :background base00-prop :weight 'bold)

  ;; Face used for leading chars.
  (set-face-attribute 'avy-lead-face-2 nil :foreground base0C-prop
                      :background base00-prop :weight 'bold)

  ;; Face used for the leading chars.
  (set-face-attribute 'avy-lead-face nil :foreground base0A-prop
                      :background base00-prop :weight 'bold)

  ;; Face for foreground/font during selection: base03.
  (set-face-foreground 'avy-background-face base03-prop))


;;; WHOLE-LINE-OR-REGION


;; Convenience: Instead of marking the line, M-x and C-x remove the
;; whole line automatically if there is no region.

(leaf whole-line-or-region

  :disabled t

  :config

  (whole-line-or-region-global-mode 1))


;;; WHICH-KEY


;; Provides a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.

(leaf which-key

  :ensure t

  :diminish which-key-mode

  :config

  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0)

  (which-key-mode 1))


;;; HIGHLIGHTING PARENTHESES and SMARTPARENS


;; Sources:
;; * https://github.com/rejeep/emacs/blob/master/init.el

(leaf parens

  :custom ((show-paren-delay . 0.0)
	   (show-paren-mode . t)))


(leaf smartparens

  :doc "Good setup: https://github.com/conao3/dotfiles/commit/d9c0f0dc55e7c65517b2c9ce8eb01f96a425ffd1#diff-f48385f05c9a82908d8bd23c391bbbd3"

  :ensure t

  :leaf-defer nil

  :diminish smartparens-mode smartparens-global-mode

  :require smartparens-config

  :bind ("C-c d p" . sp-unwrap-sexp)

  :custom

  ((sp-highlight-pair-overlay . nil)
   (smartparens-global-mode . t)))



;;; COMPANY


(leaf *company-setup

  :config

  (leaf company

    :ensure t

    :ensure company-math

    :diminish company-mode

    :config

    (global-company-mode 1)

    ;; The first variable is used to skip the downcase that company does
    ;; to the variables, the second one removes the delay.
    (setq company-dabbrev-downcase nil
	  company-idle-delay 0
	  company-tooltip-align-annotations t
	  company-show-numbers nil
	  company-minimum-prefix-length 1)

    ;; Global activation of the Unicode symbol completion.
    (add-to-list 'company-backends 'company-math-symbols-unicode))

  (leaf company-bibtex

    :ensure t

    :after company bibtex

    :config

    ;; Add backend for company-bibtex.
    (add-to-list 'company-backends 'company-bibtex)

    ;; The regular expression matching key names alphanumeric characters,
    ;; dashes (-), and underscores (_). This is customizable via:
    (setq company-bibtex-key-regex "[[:alnum:]+_]*")))


;;; AGGRESSIVE-INDENT


;; Turn aggressive-indent on for everything.

(leaf aggressive-indent

  :ensure t

  :config

  (global-aggressive-indent-mode 1))


;;; WHITESPACE


;; Make sure that there is a single additional line at the end of the file
;; while saving, also removes all white space at the end of lines.

(leaf whitespace

  :ensure t

  :after base16-theme

  :diminish whitespace-mode

  :hook ((before-save-hook . delete-trailing-whitespace)
         (prog-mode-hook . (lambda () (whitespace-mode 1)))
         (text-mode-hook . (lambda () (whitespace-mode 1)))
         (org-mode-hook . (lambda () (whitespace-mode 0)))
         (message-mode-hook . (lambda () (whitespace-mode 0))))

  :config

  ;; Set the max. column as defined above and delete trailing lines.
  (setq whitespace-line-column my-max-columns
	delete-trailing-lines t)

  ;; Define whitespace stylization.
  (setq whitespace-style '(face newline lines-tail trailing))

  ;; Change colors of text that exceeds 78 columns.
  (set-face-attribute 'whitespace-line nil
                      :foreground base08-prop :background base00-prop
                      :underline nil))


;;; MULTIPLE CURSORS


;; Basic bindings for multiple-cursors.
(leaf multiple-cursors

  :ensure t

  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C->" . mc/mark-all-like-this)))


;;; FLYCHECK


(leaf flycheck

  :ensure t

  :diminish global-flycheck-mode flycheck-mode


  :bind (("M-n" . flycheck-next-error)
	 ("M-p" . flycheck-previous-error))

  :custom ((global-flycheck-mode . t)))


;;; PYTHON-MODE


(leaf python

  :ensure conda sphinx-doc python-docstring lsp-mode

  :commands python-mode

  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode)))


;; FLYCHECK FOR PYTHON


(leaf flycheck-pycheckers

  :ensure t

  :after flycheck python

  :preface

  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

  :config

  (setq flycheck-pycheckers-multi-thread "true"
	flycheck-pycheckers-max-line-length 88 ; Follow Black guidelines.
	flycheck-pycheckers-checkers '(pylint flake8 mypy3 bandit)
	flycheck-pycheckers-ignore-codes (append
					  flycheck-pycheckers-ignore-codes
					  '("C0330" "W503" "E701" "B311"))))


;;; PDF-TOOLS


;; For better viewing and handling of PDFs in Emacs.

(leaf pdf-tools

  :ensure t

  :init

  (pdf-loader-install)

  :bind

  (pdf-view-mode-map
   ("C-s" . isearch-forward))

  :custom

  ((pdf-view-display-size . 'fit-page)
   (pdf-annot-activate-created-annotations . t))

  :config

  (pdf-loader-install))


;;; ORG-MODE

(leaf *org-setup

  :config

  (leaf org				; TODO: Use :bind at some point.

    :preface

    (prog1 "Key bindings for org"
      (global-set-key (kbd "C-c a") 'org-agenda)
      (global-set-key (kbd "C-c c") 'org-capture)
      (global-set-key (kbd "C-c l") 'org-store-link)
      (global-set-key (kbd "C-c b") 'crossref-add-bibtex-entry))

    ;; :bind

    ;; (("C-c a" . org-agenda)		  ; Call org-agenda.
    ;;  ("C-c c" . org-capture)		  ; Org-capture notes.
    ;;  ("C-c l" . org-store-link)		  ; Store link.
    ;;  ("C-c b" . crossref-add-bibtex-entry)) ; Search/add .bib entries.

    :hook

    ;; Align tags when saving.
    ((before-save-hook . org-align-all-tags)

     ;; Switch to DONE when sub-entries are done.
     (org-after-todo-statistics-hook . org-summary-todo)

     ;; Highlight current line in agenda.
     (org-agenda-mode-hook . (lambda () (hl-line-mode 1))))

    :custom

    ;; Directories.
    ((org-directory . "~/gitdir/orgdir")
     (org-default-notes-file . "~/gitdir/orgdir/notes.org")
     (org-agenda-files . "~/gitdir/orgdir")

     ;; Use relative paths.
     (org-link-file-path-type . 'relative)

     ;; Startup options.
     (org-startup-indented . t)
     (org-startup-with-latex-preview . t)
     (org-startup-align-all-tables . t)

     ;; Indentation.
     (org-indent-mode-turns-on-hiding-stars . t)
     (org-adapt-indentation . nil)

     ;; Misc.
     (org-src-window-setup . 'other-window)
     (org-tags-column . 70)
     (org-image-actual-width . nil)
     (org-highlight-latex-and-related . '(latex script entities))
     (org-catch-invisible-edits . t)

     ;; All child tasks have to be "DONE" before the parent is "DONE."
     (org-enforce-todo-dependencies . t)

     ;; To-do settings.
     (org-todo-keywords . '((sequence "TODO(t)"
				      "IN_PROGRESS(i)"
				      "GET_FEEDBACK(f)"
				      "|"
				      "DONE(d)")))
     (org-hierarchical-todo-statistics . nil)

     ;; Logging.
     (org-log-done . 'time)
     (org-log-done-with-time . t)
     (org-log-repeat . 'time)

     ;; Better calendar settings: Include last week only if today is Monday,
     ;; always show three weeks. and always start the week on Monday.
     (calendar-week-start-day . 0)
     (org-agenda-start-on-weekday . t)
     (org-agenda-start-day . "-3d")
     (org-agenda-span . 21))

    :config

    ;; Switch entry to DONE when all subentries are done, to TODO otherwise.
    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done-with-time org-log-states)   ; turn off logging
	(org-todo (if (= n-not-done 0) "DONE" "TODO"))))


    ;; FORMATTING


    ;; Always insert blank line before headings.
    (setq org-blank-before-new-entry '((heading . auto)
				       (plain-list-item . auto)))



    ;; ORG-REFILE

    (leaf *org-refile)


    ;; (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
    (setq org-refile-targets '((nil :maxlevel . 9)
			       (org-agenda-files :maxlevel . 9))
	  org-refile-use-outline-path 'file
	  org-outline-path-complete-in-steps nil
	  org-refile-allow-creating-parent-nodes 'confirm)


    ;; ORG-CAPTURE-TEMPLATES

    (setq org-capture-templates
	  '(;; Key, name, type, target, template, options.
	    ("n" "Save Note" entry
	     (file+headline "~/gitdir/orgdir/notes.org" "UNSORTED")
	     "* TODO \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:created: %U\n:END:\n\n%i\n\n"
	     :empty-lines 1
	     :prepend 1)

	    ;; Key, name, type, target, template, options.
	    ("u" "Store URL" entry
	     (file+headline "~/gitdir/orgdir/notes.org" "UNSORTED")
	     "* TODO \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:created: %U\n:END:\n\nURL: %x\n\n%i\n\n"
	     :empty-lines 1
	     :prepend 1)

	    ;; --- TEMPLATES FOR MY TO-DO LIST ---

	    ("m" "My list")

	    ;; Key, name, type, target, template, options.
	    ("mt" "TODO" entry
	     (file "~/gitdir/orgdir/todo.org")
	     "* TODO \[\#B\] %^{Title} %^g\n:PROPERTIES:\n:created: %U\n:END:\n\n%i\n\n"
	     :empty-lines 1
	     :prepend 1)

	    ;; Key, name, type, target, template, options.
	    ("ms" "Edit/fix script" entry
	     (file "~/gitdir/orgdir/todo.org")
	     "* TODO \[\#B\] %^{Title} %^g\n:PROPERTIES:\n:created: %U\nLINK: %a\n:END:\n\n%i\n\n"
	     :empty-lines 1
	     :prepend 1)

	    ;; Key, name, type, target, template, options.
	    ("mc" "Save URL and check later" entry
	     (file "~/gitdir/orgdir/todo.org")
	     "* TODO \[\#B\] %^{Title} %^g\n:PROPERTIES:\n:created: %U\n:END:\n\nURL: %x\n\n%i\n\n"
	     :empty-lines 1
	     :prepend 1)))

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

    ;; Use Python 3
    (setq org-babel-python-command "python3"))


  (leaf org-ref

    :ensure t

    :after org

    :bind (bibtex-mode-map
	   ("C-c C-c" . org-ref-clean-bibtex-entry))

    :init

    (progn
      (setq reftex-default-bibliography '("~/gitdir/library/bibliography.bib"))
      (setq org-ref-default-bibliography '("~/gitdir/library/bibliography.bib")
	    org-ref-bibliography-notes "~/gitdir/library/notes.org"
	    org-ref-pdf-directory "~/gitdir/library/archive/")

      ;; Add "bibtex %b" to enable bibtex export.
      (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

    :config

    (progn

      (setq org-ref-default-citation-link "parencite")
      ;; bibtex-dialect 'biblatex)

      ;; (setq helm-bibtex-pdf-open-function
      ;;       (lambda (fpath)
      ;;         (start-process "open" "*open*" "open" fpath)))
      ;; (setq helm-bibtex-notes-path "~/Documents/Bibliographie/Recherche/notizen.org")))
      ))


  ;; ORG-NOTER


  ;; Org-noter's purpose is to let you create notes that are kept in sync when
  ;; you scroll through the document, but that are external to it - the notes
  ;; themselves live in an Org-mode file.

  ;; Sources:
  ;; * https://github.com/weirdNox/org-noter

  ;; Also a more detailed setup: https://write.as/dani/notes-on-org-noter

  (leaf org-noter

    :ensure t

    :after org

    :config

    (setq org-noter-separate-notes-from-heading t))


  ;; ORG-JOURNAL


  (leaf org-journal

    :disabled t

    :after org

    ;; :ensure nil

    :config

    (setq org-journal-directory "~/gitdir/journal/")
    (setq org-journal-date-format "%Y-%m-%d, %A")))


;;; MAGIT


(leaf *git-tools

  :config

  (leaf magit

    :ensure t

    :require magit-todos

    :hook (magit-status-mode-hook . magit-todos-mode)

    :bind (("C-c g s" . magit-status)
	   ("C-c g c" . magit-clone)
	   ("C-c g b" . magit-blame)
	   ("C-c g d" . magit-dispatch)
	   ("C-c g a" . magit-commit-amend)))


  (leaf gitconfig-mode

    :ensure t)


  (leaf gitignore-mode

    :ensure t)


  (leaf gitattributes-mode

    :ensure t)


  (leaf git-timemachine

    :doc "https://gitlab.com/pidu/git-timemachine"

    :ensure t

    :diminish git-timemachine-mode

    :bind (("C-c g t" . git-timemachine-toggle))))


;;; LSP


(leaf lsp-mode

  :ensure t

  :require projectile company yasnippet

  :hook (python-mode-hook . lsp)

  :bind (lsp-mode-map
	 (("C-c r p" . lsp-rename)
	  ("C-c f r" . lsp-find-references)
	  ("C-c f d" . lsp-find-definition)
	  ("C-c w d" . xref-find-definitions-other-window)
	  ("C-c d p" . lsp-describe-thing-at-point)
	  ("C-c i m" . helm-imenu)))


  :custom ((lsp-inhibit-message . nil)
  	   (lsp-message-project-root-warning . t)

	   ;; Debugging.
  	   (lsp-print-io          . t)
  	   (lsp-trace             . t)
  	   (lsp-print-performance . t)

	   ;; Customization.
  	   (lsp-enable-symbol-highlighting . t)
  	   (lsp-prefer-flymake . nil)
  	   (lsp-auto-guess-root . t)
  	   (lsp-enable-snippet . nil))

  :config

  ;; Define faces for highlighting in LSP.
  (set-face-attribute 'lsp-face-highlight-write nil :italic nil :underline nil :inherit
                      'unspecified :background base02-prop :inverse-video t)
  (set-face-attribute 'lsp-face-highlight-read nil :italic nil :underline nil :inherit
                      'unspecified :background base02-prop))


;;; COMPANY-LSP


(leaf company-lsp

  :ensure t

  :after lsp company

  :commands company-lsp

  :config

  (push 'company-lsp company-backends)

  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil
        company-lsp-enable-snippet t))


;;; LSP-UI


(leaf lsp-ui

  :disabled t

  :commands lsp-ui-mode

  :hook (lsp . lsp-ui-mode)

  :config

  (setq lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-use-childframe nil
        lsp-ui-sideline-mode nil))


;;; HELM-LSP


(leaf helm-lsp

  :disabled t

  ;; :ensure nil

  :commands helm-lsp-workspace-symbol)


  ;;; PIPENV


(leaf pipenv

  :ensure t

  :after python

  :hook (python-mode-hook . pipenv-mode)

  :init

  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))


  ;;; PYTHON-PYTEST


;; Great defaults: https://shahinism.com/en/posts/emacs-python-pytest/

(leaf python-pytest

  :ensure t

  :after python

  :bind (python-mode-map
         ("C-c t p t" . python-pytest)
         ("C-c t p p" . python-pytest-popup)
         ("C-c t p f" . python-pytest-file)
         ("C-c t p F" . python-pytest-file-dwim)
         ("C-c t p d" . python-pytest-function)
         ("C-c t p D" . python-pytest-function-dwim)
         ("C-c t p l" . python-pytest-last-failed))

  :custom

  (python-pytest-arguments
   '("--color"                        ; Colored output in the buffer.
     "--pdb"                          ; Run pdb on failure.
     "--verbose"                      ; More verbose output.
     ;; "--failed-first"                 ; Run the previous failed tests first.
     ;; "--exitfirst"                    ; Exit after first failure.
     ;; "--maxfail=5"; Exit in 5 continuous failures in a run.
     ))

  (python-pytest-pdb-track t))


  ;;; PYTHON COVERAGE


(leaf pycoverage

  :disabled t

  :after python

  :config

  (defun my-coverage ()
    (interactive)
    (when (derived-mode-p 'python-mode)
      (progn
        (pycoverage-mode)))))


  ;;; SPHINX-DOC


(leaf sphinx-doc

  :ensure t

  :after python

  :load-path "~/gitdir/sphinx-doc.el/"

  :hook (python-mode-hook . sphinx-doc-mode)

  :config

  ;; Show all arguments (except "self").
  (setq sphinx-doc-all-arguments t)
  (setq sphinx-doc-exclude-rtype t))


;;; CONDA


(leaf conda

  :ensure t

  :after projectile

  :config

  ;; Set Conda directory.
  (custom-set-variables
   '(conda-anaconda-home "~/miniconda3/"))

  ;; Interactive shell support, include.
  (conda-env-initialize-interactive-shells)

  ;; Eshell support.
  (conda-env-initialize-eshell)

  ;; Auto-activation.
  (setq conda-env-autoactivate-mode nil))


  ;;; BLACK FORMATTER


(leaf blacken

  :disabled t

  :after python

  :hook (python-mode-hook . blacken-mode))


;;; PY-ISORT


(leaf py-isort

  :disabled t t

  :hook (before-save-hook . py-isort-before-save))


;;; PYTHON-DOCSTRING


(leaf python-docstring

  :ensure t

  :after python

  :hook (python-mode-hook . python-docstring-mode))


;;; ELPY


;; TODO: Remove at some point.


(leaf elpy

  :disabled t

  :after python

  :init

  ;; If elpy-enable is off, enable elpy.
  (when (require 'elpy nil t)
    (elpy-enable))

  :bind (elpy-mode-map
         ("C-c C-g C-d" . elpy-goto-definition-other-window))

  :hook (python-mode-hook . elpy-mode)

  :config

  (setq elpy-test-pytest-runner-command '("py.test" "-c" "--pdb" "-x"))

  ;; RPC backend for elpy.
  (setq elpy-rpc-python-command "python3")

  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")
  ;; python-shell-interpreter-args "console --simple-prompt")
  ;; python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;;              "jupyter")
  (setq gud-pdb-command-name "python3 -m pdb") ; Using pdb.

  (setq python-shell-completion-native-enable nil)
  (setq python-indent-offset 4)         ; Indent with 4 spaces.
  (setq scroll-down-aggressively 1)     ; Not sure what this does.

  ;; Add Company-jedi to python-mode.
  (add-hook 'elpy-mode-hook
            (lambda () (add-to-list 'company-backends 'company-jedi)))

  ;; Jedi settings.
  ;; (defun my/python-mode-hook ()
  ;;   (add-to-list 'company-backends 'company-jedi))
  ;; (add-hook 'python-mode-hook 'my/python-mode-hook)

  ;; (setq elpy-rpc-backend "jedi")        ; Use Jedi as backend.
  ;; (setq jedi:complete-on-dot t)
  ;; (setq jedi:use-shortcuts t)

  ;; ;; Solve company, yasnippet conflicts.
  ;; (defun company-yasnippet-or-completion ()
  ;;   "Solve company yasnippet conflicts."
  ;;   (interactive)
  ;;   (let ((yas-fallback-behavior
  ;;          (apply 'company-complete-common nil)))
  ;;     (yas-expand)))

  ;; (add-hook 'company-mode-hook
  ;;           (lambda ()
  ;;             (substitute-key-definition
  ;;              'company-complete-common
  ;;              'company-yasnippet-or-completion
  ;;              company-active-map)))


  ;; Fix the yasnippet issue with the following function.
  ;; (defun company-yasnippet-or-completion ()
  ;;   "Solve company yasnippet conflicts."
  ;;   (interactive)
  ;;   (let ((yas-fallback-behavior
  ;;          (apply 'company-complete-common nil)))
  ;;     (yas-expand)))
  ;; (add-hook 'company-mode-hook
  ;;           (lambda ()
  ;;             (substitute-key-definition
  ;;              'company-complete-common
  ;;              'company-yasnippet-or-completion
  ;;              company-active-map)))

  )


;;; LATEX/AUCTEX


(leaf auctex

  :disabled t

  ;; :ensure nil

  :hook ((LaTeX-mode-hook . turn-on-reftex)
         (LaTeX-mode-hook . LaTeX-math-mode))

  :config

  ;; (unload-feature 'tex-site) ; Remove a preinstalled AUCTeX completely
  ;; before ; any of its modes have been used.

  (latex-preview-pane-enable)

  ;; Basic setup.
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-master nil)

  (setq TeX-show-compilation nil) ; If t, automatically shows compilation log.
  (setq TeX-save-query nil)       ; Don't prompt for saving the .tex file.


  ;; Misc. Latex settings.
  ;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  ;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  ;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))


;;; EMACS SPEAKS STATISTICS (ESS)


(leaf ess

  :ensure t

  :commands R

  :config

  ;; Always split window vertically.
  ;; (add-hook 'ess-mode-hook
  ;;           (lambda()
  ;;             (setq-local split-height-threshold nil)
  ;;             (setq-local split-width-threshold  0)
  ;;             ))

  ;; Load Rnw-mode and Snw-mode first.
  ;; (add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.Snw\\'" . Snw-mode))

  ;; Always whitespace after comma.
  (setq ess-R-smart-operators nil)

  ;; Don't restore history and don't save history.
  (setq inferior-R-args "--no-restore-history --no-save")

  ;; Start R in the working directory by default.
  (setq ess-ask-for-ess-directory nil)

  ;; ESS will not print the evaluated commands, also speeds up the
  ;; evaluation.
  (setq ess-eval-visibly nil)

  ;; Better inferior mode settings: scroll down inferior with input/output.
  (setq comint-scroll-to-bottom-on-input t
	comint-scroll-to-bottom-on-output t
	comint-move-point-for-output t)

  ;; Major mode font-lock.
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers .t )
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . nil)))

  ;; Inferior mode font-lock.
  (setq inferior-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . nil))))


;;; POLY-MODE


(leaf poly-markdown

  :disabled t

  ;; :ensure poly-R

  :mode (("\\.md$" . poly-markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.Snw$" . poly-noweb+R-mode)
         ("\\.Rnw$" . poly-noweb+R-mode)
         ("\\.Rmd$" . poly-markdown+R-mode)
         ("\\.rapport$" . poly-rapport-mode)
         ("\\.Rhtml$" . poly-html+R-mode)
         ("\\.Rbrew$" . poly-brew+R-mode)
         ("\\.Rcpp$" . poly-R+C++-mode)
         ("\\.cppR$" . poly-C++R-mode))

  :hook (poly-markdown-mode . display-line-numbers-mode))


;;; YASNIPPET


(leaf yasnippet

  :ensure t

  :ensure yasnippet-snippets

  :bind (("C-c y" . yas-insert-snippet)
         ("C-c v" . yas-visit-snippet-file))

  :init

  (yas-global-mode 1)

  :config

  (setq yas-snippet-revival nil
        yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/gitdir/emacs-init/snippets/"))))



;;; GNUPG SETUP


(leaf pinentry

  :ensure t

  :config

  (pinentry-start)

  ;; Always use GPG2 and use loopback option for better compatablilty.
  (setq epg-gpg-program "gpg2"
        epa-pinentry-mode 'loopback))


;;; MU4E/MAILS


;; Only load mu4e when path to repository exists.

(prog1 "Load mu4e setup"
  (let ((mu4e-setup "~/gitdir/mu4e-setup/mu4e-setup.el"))
    (when (file-exists-p mu4e-setup)
      (load mu4e-setup))))


;;; OPENWITH


(leaf openwith

  :disabled t

  ;; :ensure nil

  ;; Change default programs for specific files.
  :hook ((text-mode-hook . (lambda () (openwith-mode 1)))
	 (dired-mode-hook . (lambda () (openwith-mode 1)))
	 (message-mode-hook . (lambda () (openwith-mode 0)))
	 (markdown-mode-hook . (lambda () (openwith-mode 1))))

  :config

  (openwith-mode 1)                     ; Activate/deactivate openwith-mode by
                                        ; default.

  ;; Manually define file associations.
  (when (eq system-type 'gnu/linux)
    (if (string-equal
         (substring (shell-command-to-string "lsb_release -sd") 0 3)
         (substring "Arch" 0 3))
        (setq openwith-associations '(("\\.png\\'" "feh" (file))
                                      ("\\.jpg\\'" "feh" (file)f)
                                      ("\\.docx\\'" "libreoffice" (file))
                                      ("\\.odt\\'" "libreoffice" (file))
                                      ;; ("\\.pdf\\'" "evince" (file))
                                      ("\\.html\\'" "GDK_BACKEND=wayland firefox --new-window" (file))
                                      ("\\.htm\\'" "GDK_BACKEND=wayland firefox --new-window" (file))
                                      ("\\.mkv\\'" "mplayer" (file))))
      (setq openwith-associations '(("\\.png\\'" "feh" (file))
                                    ("\\.jpg\\'" "feh" (file)f)
                                    ("\\.docx\\'" "libreoffice" (file))
                                    ("\\.odt\\'" "libreoffice" (file))
                                    ;; ("\\.pdf\\'" "evince" (file))
                                    ("\\.html\\'" "GDK_BACKEND=wayland firefox --new-window" (file))
                                    ("\\.htm\\'" "GDK_BACKEND=wayland firefox --new-window" (file))
                                    ("\\.mkv\\'" "mplayer" (file))))
      )))


;;; GSCHOLAR-BIBTEX


;; "Google Scholar" as default source and write to bibliography.bib
;; directly.

(leaf gscholar-bibtex

  :disabled t

  ;; :ensure nil

  :config

  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  (setq gscholar-bibtex-default-source "Google Scholar"))
;; gscholar-bibtex-database-file "~/gitdir/bibliography/bibliography.bib"))


;;; JUPYTER NOTEBOOK


;; Seems buggy...

(leaf ein

  :disabled t

  ;; :ensure nil

  :config

  ;; Work-around for proxy issues.  Not sure if this works.
  (setq request-curl-options '("--noproxy" "127.0.0.1:8888")))


;;; TYPIT


;; This is a typing game for Emacs. In this game, you type words that are
;; picked randomly from N most frequent words in language you're practicing,
;; until time is up (by default it's one minute). Typit is quite similar to
;; the "10 fast fingers" tests.

(leaf typit

  :ensure t)


;;; YAML-MODE


(leaf yaml-mode

  :config

  :bind (yaml-mode-map
         ("\C-m" . newline-and-indent)))


;;; CSV-MODE


(leaf csv-mode

  :config

  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
  (autoload 'csv-mode "csv-mode"
    "Major mode for editing comma-separated value files." t))


;;; EDIFF


(leaf ediff

  :config

  ;; Don't start another frame.
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Put windows side by side.
  (setq ediff-split-window-function (quote split-window-horizontally))
  ;; Revert windows on exit - needs winner mode
  (winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))


;;; PROJECTILE


(leaf projectile

  :after helm

  :ensure helm-projectile

  :bind ("C-c p" . projectile-command-map)

  :init

  (projectile-mode 1)                   ; Activate Projectile.
  (helm-projectile-on)                  ; Turn on helm-projectile keys.

  :config

  ;; Use helm as completion system for Projectile.
  (setq projectile-completion-system 'helm))


;;; TRAMP FOR REMOTE FILE SYSTEMS


(leaf tramp

  :disabled t

  ;; :ensure nil

  :ensure helm-tramp

  :after helm

  :bind ("C-c t h" . helm-tramp)

  :config

  ;; (setq tramp-default-method "ssh")

  (setq tramp-verbose 10))


;;; REALGUD DEBUGGER


(leaf realgud

  :disabled t

  ;; :ensure nil

  :commands realgud:ipdb

  :config

  ;; Does not seem to work.
  (setq realgud:ipdb-command-name "/bin/ipdb3")
  (setq realgud:pdb-command-name "python3 -m pdb"))


;;; Footer:


(provide 'init)
;;; init.el ends here
