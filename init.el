;;; init.el --- Emacs initialization file -*- coding: utf-8 -*-


;; Copyright (C) 2018-2019 Karsten E. Beismann

;; Author: Karsten Beismann
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
;; possible.  It currently uses "leaf" to create a modular system.  Dependent
;; on the system, the bitmap fonts have to be installed and configurated
;; upfront.  Note that Pango removed support for bitmap fonts in version 1.33.

;; The some settings, e.g. the mu4e setup, are not part of this file.


;;; Some working font options:

;; "Hack-9":
;; * "Hack:pixelsize=14"

;; "DejaVu Sans Mono-9":
;; * "DejaVu Sans Mono-10"

;; "Inconsolata-11":
;; * "Inconsolata:pixelsize=14"

;; "Dina-9":
;; * "Dina:pixelsize=12"
;; * ;; "-*-dina-medium-r-*-*-12-*-*-*-*-*-*-*"

;; "Terminus-12":
;; * "xos4 Terminus-10": Arch
;; * "-*-terminus-medium-r-normal-*-14-*-*-*-*-*-*-*"
;; * "-xos4-terminus-medium-r-normal-*-14-120-*-*-*-*-*-*"
;; * "-xos4-terminus-medium-r-normal--16.5-120-*-*-*-*-*-*": Ubuntu.


;;; Sources (incomplete):

;; * https://github.com/rememberYou/.emacs.d/blob/master/config.org/#python
;; * https://github.com/conao3/dotfiles/blob/master/.dotfiles/.emacs.d/init.el
;; * https://github.com/conao3/dotfiles/blob/master/.dotfiles/.emacs.d/init.el


;;; To-do:

;; * Init file in org-mode?
;; * Better YASnippet implementation.
;; * Use function/package to toggle proxies.


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


;; WORK-RELATED SETTINGS


;; Load proxy settings for work.

(prog1 "Load proxy if file exists"
  
  (let ((proxies "~/gitdir/emacs-work/proxies.el"))
    (when (file-exists-p proxies)
      (load proxies))))


;;; BASIC SETTINGS


(setq user-full-name "Karsten Beismann")

;; Columns settings and M-q behavior.
(defvar max-columns 78)                 ; Global column setting, which will be
                                        ; parsed to every template.
(setq-default fill-column max-columns   ; Set M-q columns.
              truncate-lines t          ; No line-wrapping
              line-spacing nil)         ; Spacing between lines.

;; Change save and buffer updating behavior.
(blink-cursor-mode 1)               ; Use blinking cursor.
(desktop-save-mode 0)               ; Don't save last Emacs session.
(save-place-mode 1)                 ; Save the place in files.
(global-auto-revert-mode t)         ; Revert buffers automatically.

;; Important key-bindings and shortcuts.
(global-unset-key (kbd "C-x C-z"))             ; Unbind suspend frame.
(global-set-key (kbd "S-SPC") 'just-one-space) ; Unbind suspend frame.
(fset 'yes-or-no-p 'y-or-n-p)                  ; y/p instead of yes/no.

;; Minor customization.
(setq ring-bell-function 'ignore             ; No annoying bell.
      debug-on-error t                       ; Always debug.
      init-file-debug t                      ; Debug init file as well.
      inhibit-startup-screen t               ; No starting screen.
      large-file-warning-threshold 100000000 ; Large file warning.
      next-line-add-newlines t               ; New line when C-n.
      mouse-yank-at-point t                  ; Paste at cursor, not at mouse.
      indent-tabs-mode nil)                  ; Always indent with spaces.
                                        ;
;; Clipboard behavior.
;; (setq x-select-enable-clipboard-manager nil)   ; Seems to work well.

;; (setq tab-always-indent 'complete) ; Use Tab to Indent or Complete.

;; Better scrolling behavior.
(setq  scroll-margin 0
       scroll-conservatively 10000
       scroll-preserve-screen-position nil
       auto-window-vscroll nil)

;; Better inferior mode settings: scroll down inferior with input/output.
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-move-point-for-output t)

;; Visual changes.
(setq default-frame-alist '((vertical-scroll-bars . nil)  ; No scroll bar.
                            (menu-bar-lines       . nil)  ; No menu bar.
                            (tool-bar-lines       . nil)) ; No tool bar.
      font-lock-maximum-decoration t)   ; Decorate as much as possible.


;; Frame settings dependent on OS.
(when (eq system-type 'gnu/linux)
  (when (string-equal
         (substring (shell-command-to-string "lsb_release -sd") 0 3)
         (substring "Ubun" 0 3))
    (progn (add-to-list 'default-frame-alist '(height . 50))
           (add-to-list 'default-frame-alist '(width . 200))))
  (when (string-equal
         (substring (shell-command-to-string "lsb_release -sd") 0 3)
         (substring "Manj" 0 3))
    (progn (add-to-list 'default-frame-alist '(height . 200))
           (add-to-list 'default-frame-alist '(width . 300)))))


;; FONTS


;; Font settings, dependent on the OS.
(when (eq system-type 'gnu/linux)
  (if (string-equal
       (substring (shell-command-to-string "lsb_release -sd") 0 3)
       (substring "Ubun" 0 3))
      (add-to-list 'default-frame-alist
                   '(font . "-xos4-terminus-medium-r-normal--16.5-120-*-*-*-*-*-*"))
    (add-to-list 'default-frame-alist '(font . "Dina:pixelsize=12"))
    ))


;;; BACKUPS/ABBREVS/LOCKFILES/CUSTOMIZE


;; Prevent custom-file from littering the init file.
(setq custom-file "~/.custom.el")
(load custom-file)

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
      '((".*" "~/emacsauto/autosaves" t)))


;;; AUTOMATIC TIME STAMPS


(setq time-stamp-active t ; Enable time-stamping.
      time-stamp-format
      "%04y-%02m-%02dT%02H:%02M:%02S, %u@%s") ; ISO time-stamping format.
;; (add-hook 'write-file-hooks 'time-stamp) ; Automatically update when saving.


;;; LINE NUMBERING


;; The display-line-numbers colors can be changed by editing base16.el

;; Enable line numbers in prog-mode.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Configure the line numbers on the left.
(setq-default display-line-numbers nil      ; Don't show line numbers.
              display-line-numbers-width 4  ; Default width.
              display-line-numbers-widen t) ; Don't disregard narrowing.


;;; MISC. FUNCTIONS


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
(global-set-key (kbd "C-S-s") 'find-first-non-ascii-char)


;;; MODE LINE


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
        (line-number-mode ("%l" (column-number-mode ":%c")))))


;; DIRECTORY SHORTENING

;; (prog1 "Better directory in mode-line"

;;   ;; Before the buffer-ID, I want to place the directory. But not the
;;   ;; whole directory, but shortened version. (I didn't write this
;;   ;; function, I have it from some internet web page, unsure from
;;   ;; where. It's used quite often, try too google for "defun
;;   ;; shorten-directory" ...)

;;   (defun shorten-directory (dir max-length)
;;     "Show up to MAX-LENGTH characters of a directory name DIR."
;;     (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
;;           (output ""))
;;       (when (and path (equal "" (car path)))
;;         (setq path (cdr path)))
;;       (while (and path (< (length output) (- max-length 4)))
;;         (setq output (concat (car path) "/" output))
;;         (setq path (cdr path)))
;;       (when path
;;         (setq output (concat ".../" output)))
;;       output))


;; ;;; DIRECTORY NAME


;;   ;; Some buffers however don't have files (and thus no directories, too),
;;   ;; e.g. the *scratch* buffer. In such cases the result of
;;   ;; (buffer-file-name) is nil. In such a case I omit the shortened
;;   ;; directory.

;;   ;; But no matter if we emit a directory name, we always emit a starting
;;   ;; space. This space get's propertized with the rest, and let our
;;   ;; directory / buffer-id stand out nicely.

;;   (defvar mode-line-directory
;;     '(:propertize
;;       (:eval (if (buffer-file-name)
;;                  (concat " " (shorten-directory default-directory 20)) " "))
;;       face mode-line-directory)
;;     "Formats the current directory.")
;;   (put 'mode-line-directory 'risky-local-variable t)

;;   ;; Having an empty space before directory part calls for an space after
;;   ;; the buffer ID.
;;   (setq-default mode-line-buffer-identification
;;                 (propertized-buffer-identification "%b "))


;; ;;; DEFINE THE MODE LINE


;;   ;; This defines the elements of the mode line, using the functions
;;   ;; created above.

;;   (setq-default mode-line-format
;;                 '("%e"
;;                   mode-line-front-space
;;                   ;; mode-line-mule-info ; I'm always on utf-8.
;;                   mode-line-client
;;                   mode-line-modified
;;                   mode-line-remote ; No need to indicate this specially
;;                                         ; but might be useful for working with
;;                                         ; servers?
;;                   mode-line-frame-identification ; This is for text-mode
;;                   ;; emacs only.
;;                   " "
;;                   ;; mode-line-directory ; Defined above. NOT WORKING PROPERLY
;;                   ;; RIGHT NOW.
;;                   mode-line-buffer-identification ; Name of current buffer.
;;                   " "
;;                   mode-line-position ; Line and Column number
;;                   ;; (vc-mode vc-mode) ; Use magit if needed, not vc-mode.
;;                   (flycheck-mode flycheck-mode-line)
;;                   " "
;;                   ;; mode-line-modes ; Try without but might be useful.
;;                   git-status-in-modeline
;;                   mode-line-misc-info
;;                   mode-line-end-spaces)))


;;; LEAF SETUP


(prog1 "Use leaf to simplify package management"

  ;; Add archives and assign priorities.
  ;; (setq package-check-signature nil)    ; Do/don't check sig. ; TODO: remove this.
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
  
  (require 'leaf)
  
  ;; (leaf leaf

  ;;   :doc "Simplify Emacs configuration"

  ;;   :commands leaf

  ;;   :config

  (leaf leaf-keywords

    :ensure t

    :config

    (leaf diminish

      :ensure t)

    ;; Initialize leaf-keywords.el.
    (leaf-keywords-init)))


;;; AUTO-PACKAGE-UPDATE


;; Updates packages regularly.

(leaf auto-package-update

  ;; :disabled

  :ensure t

  :config

  (setq auto-package-update-delete-old-versions t   ; Remove old files.
        auto-package-update-hide-results nil        ; Don't hide results.
        auto-package-update-prompt-before-update t)) ; Ask before updating.


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


;; I could add diredplus.

(leaf dired

  :commands dired

  :ensure async

  :hook (dired-mode-hook . (lambda() (hl-line-mode 1)))

  :config

  (setq dired-dwim-target t             ; Better default target directory.
        dired-recursive-copies 'always  ; Always copy recursively.
        dired-hide-details-hide-symlink-targets nil ; Don't hide symlinks.
        dired-listing-switches                      ; Better columns.
        "-lahgF --group-directories-first")) 


;;; ASYNC


(leaf async

  :ensure t

  :after dired

  :config

  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))


;;; ESHELL


;; Check out https://github.com/jcf/emacs.d/blob/master/init-packages.org.

(leaf eshell

  :commands eshell

  :bind ("C-z" . eshell)

  :hook (eshell-mode . my-eshell-remove-pcomplete)

  :config

  ;; Fixes weird issues in eshell.
  (defun my-eshell-remove-pcomplete ()
    (remove-hook 'completion-at-point-functions
                 #'pcomplete-completions-at-point t)))



;;; BIBTEX


(leaf bibtex

  :config

  ;; Path to library only set when directory exists.
  (let ((path-to-library "~/gitdir/library/"))
    (when (file-exists-p path-to-library)
      (setq bibtex-completion-library-path path-to-library)))

  (let ((path-to-bib "~/gitdir/library/bibliography.bib"))
    (when (file-exists-p path-to-bib)
      (setq bibtex-completion-bibliography path-to-bib)))


  ;; Autokey format.
  (setq bibtex-autokey-additional-names "_etal"
        bibtex-autokey-name-separator "_"
        bibtex-autokey-names 1
        bibtex-autokey-names-stretch 1
        bibtex-autokey-name-length 999
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-length 4
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "_"
        bibtex-autokey-titlewords 3
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))



;;; DICTIONARY, FLYCHECK, AND FLYSPELL


(leaf flyspell

  :ensure t
  
  :require t

  :leaf-defer nil

  :hook ((prog-mode . (lambda() (flyspell-prog-mode)))
         (text-mode . (lambda() (flyspell-mode)))
         
         ;; Deactivate for logs and log editing.
         (log-edit-mode . (lambda() (flyspell-mode -1)))
         (change-log-mode . (lambda() (flyspell-mode -1))))

  :init 

  (flyspell-mode)
  
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

  ;; :ensure helm-mu
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

  :disabled

  :ensure t

  :after helm bibtex

  :bind (helm-command-map
         ("b". helm-bibtex)))


;;; SET COLOR THEME


(leaf base16-theme

  :ensure t

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

  :ensure t

  :config

  (whole-line-or-region-global-mode 1))


;;; WHICH-KEY


;; Provides a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup.

(leaf which-key

  :ensure t

  :config

  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0)

  (which-key-mode 1))


;;; HIGHLIGHTING PARENTHESES and SMARTPARENS


;; Sources:
;; * https://github.com/rejeep/emacs/blob/master/init.el

(leaf parens

  :init (show-paren-mode))

(leaf smartparens

  :disabled

  ;; :ensure nil

  :bind ("C-c d p" . sp-unwrap-sexp)

  :init

  (progn (leaf smartparens-config)
         (smartparens-mode 1)
         (show-smartparens-global-mode 1)))


;;; COMPANY


(leaf company

  :ensure t

  :ensure company-math

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


;;; COMPANY FOR BIBTEX


(leaf company-bibtex

  :ensure t

  :after company bibtex

  :config

  ;; Add backend for company-bibtex.
  (add-to-list 'company-backends 'company-bibtex)

  ;; The regular expression matching key names alphanumeric characters,
  ;; dashes (-), and underscores (_). This is customizable via:
  (setq company-bibtex-key-regex "[[:alnum:]+_]*"))


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

  :hook ((before-save . delete-trailing-whitespace)
         (prog-mode . (lambda () (whitespace-mode 1)))
         (text-mode . (lambda () (whitespace-mode 1)))
         (conf-mode . (lambda () (whitespace-mode 1)))
         (org-mode . (lambda () (whitespace-mode 0)))
         (ess-mode . (lambda () (whitespace-mode 1)))
         (message-mode . (lambda () (whitespace-mode 0))))

  :config

  ;; Set the max. column as defined above.
  (setq whitespace-line-column max-columns)

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

  :commands global-flycheck-mode

  :hook ((prog-mode . flycheck-mode)
         (ess-mode . flycheck-mode)))


(leaf flycheck-pycheckers

  :ensure t

  :after flycheck python

  :init

  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

  :config

  (setq flycheck-pycheckers-multi-thread "true"
        flycheck-pycheckers-max-line-length 88) ; Accommodate Black settings.

  ;; Add linters here.
  (setq flycheck-pycheckers-checkers
        '(pylint flake8 mypy3 bandit))

  ;; Add ignorable codes here.
  (setq flycheck-pycheckers-ignore-codes
        (append flycheck-pycheckers-ignore-codes
                '("C0330" "W503" "E701" "B311"))))


;;; PDF-TOOLS


;; For better viewing and handling of PDFs in Emacs.

(leaf pdf-tools

  :ensure t

  :init

  (pdf-loader-install)

  :bind (pdf-view-mode-map
         ("C-s" . isearch-forward))

  :config

  ;; Initialize.
  ;; (pdf-loader-install)

  ;; Open PDFs scaled to fit page.
  ;; (setq-default pdf-view-display-size 'fit-page)

  ;; Automatically annotate highlights.
  ;; (setq pdf-annot-activate-created-annotations t)
  )


;;; ORG-MODE


(leaf org

  :ensure t

  :bind (("C-c c" . org-capture)               ; Org-capture notes.
         ("C-c a" . org-agenda)                ; Call org-agenda.
         ("C-c b" . crossref-add-bibtex-entry) ; Search/ add .bib entries.
         ("C-c l" . org-store-link))           ; Store link.

  :init

  (setq org-directory "~/gitdir/orgdir"
        org-default-notes-file (concat org-directory "/notes.org"))

  :config

  ;; Images shouldn't use their actual width.
  (setq org-image-actual-width nil)

  ;; Always align tags before saving.
  (add-hook 'before-save-hook 'org-align-all-tags)


  ;; TODO AND LOGGING


  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN_PROGRESS(i)" "GET_FEEDBACK(f)" "|" "DONE(d)")))

  (setq org-hierarchical-todo-statistics nil)

  (setq org-log-done 'time
        org-log-done-with-time t
        org-log-repeat 'time)

  ;; Switch entry to DONE when all subentries are done, to TODO otherwise.
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done-with-time org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Use relative paths.
  (setq org-link-file-path-type 'relative)


  ;; FORMATTING


  ;; Org-mode startup options.
  (setq org-hide-leading-stars nil
        org-startup-indented t
        org-adapt-indentation nil
        org-startup-with-latex-preview t
        org-startup-align-all-tables t)

  (setq org-tags-column 70)             ; Col. between tag and heading.

  ;; Always insert blank line before headings.
  (setq org-blank-before-new-entry
        '((heading . auto)
          (plain-list-item . auto)))

  (setq org-catch-invisible-edits t)    ; Invisible edits impossible.

  ;; Forces you to mark all child tasks as “DONE” before you can mark the
  ;; parent as "DONE."
  (setq org-enforce-todo-dependencies t)


  ;; POMODORO


  ;; Activate the org-timer module:
  (add-to-list 'org-modules 'org-timer)

  ;; Set a default value for the timer, for example:
  (setq org-timer-default-timer 25)

  ;; Modify the org-clock-in so that a timer is started with the default
  ;; value except if a timer is already started :
  (add-hook 'org-clock-in-hook (lambda ()
                                 (if (not org-timer-current-timer)
                                     (org-timer-set-timer '(16)))))


  ;; ORG-AGENDA


  ;; Set directory for notes.
  (setq org-agenda-files (list org-directory))

  ;; Highlight current line in agenda.
  (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

  ;; Better calendar settings: Include last week only if today is
  ;; Monday, always show three weeks. and always start the week on
  ;; Monday.
  (setq calendar-week-start-day 0
        org-agenda-start-on-weekday t
        org-agenda-start-day "-3d"
        org-agenda-span 21)


  ;; ORG-REFILE


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


  ;;; ORG-BABEL


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


  ;;; ORG-REF


(leaf org-ref

  :ensure t

  :require org helm

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

  :disabled

  ;; :ensure nil

  :config

  (setq org-journal-directory "~/gitdir/journal/")
  (setq org-journal-date-format "%Y-%m-%d, %A"))


;;; MAGIT


(leaf magit

  :ensure t

  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))


;;; MAGIT-TODO


(leaf magit-todos

  :ensure t

  :after magit)


;;; LSP


(leaf lsp-mode

  :ensure t

  :after projectile company yasnippet

  :hook (python-mode . lsp)

  :bind (lsp-mode-map
         (("C-c d p" . lsp-describe-thing-at-point)
          ("C-c i m" . helm-imenu)
          ("C-c f d" . lsp-find-definition)
          ("C-c w d" . xref-find-definitions-other-window)
          ("C-c f r" . lsp-find-references)
          ("C-c r p" . lsp-rename)))

  :config

  (setq lsp-enable-symbol-highlighting t ; Highlight symbols at point.
        lsp-prefer-flymake nil           ; Don't use flymake by default.
        lsp-auto-guess-root t            ; Guess the root directory if possible.
        lsp-enable-snippet t)            ; Use Yasnippets.
  ;; lsp-eldoc-enable-hover nil)
  ;; lsp-eldoc-enable-signature-help nil

  ;; Define faces for highlighting in LSP.
  (set-face-attribute 'lsp-face-highlight-write nil :italic nil :underline nil :inherit
                      'unspecified :background base02-prop :inverse-video t)
  (set-face-attribute 'lsp-face-highlight-read nil :italic nil :underline nil :inherit
                      'unspecified :background base02-prop))


;;; COMPANY-LSP


(leaf company-lsp

  :ensure t

  :after (lsp company)

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

  :disabled

  ;; :ensure nil

  :commands lsp-ui-mode

  :hook (lsp . lsp-ui-mode)

  :config

  (setq lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-use-childframe nil
        lsp-ui-sideline-mode nil))


;;; HELM-LSP


(leaf helm-lsp

  :disabled

  ;; :ensure nil

  :commands helm-lsp-workspace-symbol)


;;; PYTHON-MODE


(leaf python

  :ensure t

  :after (base16-theme yasnippet)       ; To avoid color issues.

  :ensure conda sphinx-doc python-docstring lsp-mode

  :commands python-mode

  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode)))


  ;;; PIPENV


(leaf pipenv

  :ensure t

  :after python

  :hook (python-mode . pipenv-mode)

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

  :disabled

  ;; :ensure nil

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

  :hook (python-mode . sphinx-doc-mode)

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

  :disabled

  ;; :ensure nil

  :after python

  :hook (python-mode . blacken-mode))


;;; PY-ISORT


(leaf py-isort

  :disabled

  ;; :ensure nil

  :hook (before-save . py-isort-before-save))


;;; PYTHON-DOCSTRING


(leaf python-docstring

  :ensure t

  :after python

  :hook (python-mode . python-docstring-mode))


;;; ELPY


(leaf elpy

  :disabled

  ;; :ensure nil

  :after python

  :init

  ;; If elpy-enable is off, enable elpy.
  (when (require 'elpy nil t)
    (elpy-enable))

  :bind (elpy-mode-map
         ("C-c C-g C-d" . elpy-goto-definition-other-window))

  :hook (python-mode . elpy-mode)

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

  :disabled

  ;; :ensure nil

  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode))

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


;; (leaf poly-markdown

;;   :disabled

;;   ;; :ensure poly-R

;;   :mode (("\\.md$" . poly-markdown-mode)
;;          ("README\\.md\\'" . gfm-mode)
;;          ("\\.Snw$" . poly-noweb+R-mode)
;;          ("\\.Rnw$" . poly-noweb+R-mode)
;;          ("\\.Rmd$" . poly-markdown+R-mode)
;;          ("\\.rapport$" . poly-rapport-mode)
;;          ("\\.Rhtml$" . poly-html+R-mode)
;;          ("\\.Rbrew$" . poly-brew+R-mode)
;;          ("\\.Rcpp$" . poly-R+C++-mode)
;;          ("\\.cppR$" . poly-C++R-mode))

;;   :hook (poly-markdown-mode . display-line-numbers-mode))


;;; YASNIPPET


(leaf yasnippet

  :ensure t

  :ensure yasnippet-snippets

  :bind (("C-c y" . yas-insert-snippet)
         ("C-c v" . yas-visit-snippet-file))

  :init

  (yas-global-mode 1)

  :config

  (setq yas-snippet-revival t
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
(let ((mu4e-setup "~/gitdir/mu4e-setup/mu4e-setup.el"))
  (when (file-exists-p mu4e-setup)
    (load mu4e-setup)))


;;; OPENWITH


(leaf openwith

  :disabled

  ;; :ensure nil

  ;; Change default programs for specific files.
  :hook ((dired-mode . (lambda () (openwith-mode 1)))
         (message-mode . (lambda () (openwith-mode 0)))
         (text-mode . (lambda () (openwith-mode 1)))
         (markdown-mode . (lambda () (openwith-mode 1))))

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

  :disabled

  ;; :ensure nil

  :config

  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  (setq gscholar-bibtex-default-source "Google Scholar"))
;; gscholar-bibtex-database-file "~/gitdir/bibliography/bibliography.bib"))


;;; GCAL


(leaf org-gcal

  :disabled

  ;; :ensure nil

  :init

  (setq package-check-signature nil)

  :config

  (setq org-gcal-client-id "***REMOVED***"
	org-gcal-client-secret "***REMOVED***"
	org-gcal-file-alist '(("***REMOVED***" . "~/gitdir/orgdir/gcal.org")))

  ;; Automatic synchronization when using agenda or when capturing.
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync))))


;;; JUPYTER NOTEBOOK


;; Seems buggy...

(leaf ein

  :disabled

  ;; :ensure nil

  :config

  ;; Work-around for proxy issues.  Not sure if this works.
  (setq request-curl-options '("--noproxy" "127.0.0.1:8888")))


;;; TYPIT


;; This is a typing game for Emacs. In this game, you type words that are
;; picked randomly from N most frequent words in language you're practicing,
;; until time is up (by default it's one minute). Typit is quite similar to
;; the "10 fast fingers" tests.

(leaf typit)


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

  :disabled

  ;; :ensure nil

  :ensure helm-tramp

  :after helm

  :bind ("C-c t h" . helm-tramp)

  :config

  ;; (setq tramp-default-method "ssh")

  (setq tramp-verbose 10))


;;; REALGUD DEBUGGER


(leaf realgud

  :disabled

  ;; :ensure nil

  :commands realgud:ipdb

  :config

  ;; Does not seem to work.
  (setq realgud:ipdb-command-name "/bin/ipdb3")
  (setq realgud:pdb-command-name "python3 -m pdb"))


;;; Footer:


(provide 'init)
;;; init.el ends here
