;;; early-init.el --- Early initialization file -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 Karsten E. Beismann
;; Author: Karsten Beismann
;; Homepage: https://github.com/kbeismann/emacs-init
;; Created: Tue Sep 24 21:43:39 2019 +0200
;; Package-Requires: ((emacs "28"))
;; Keywords: init, emacs, early

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; For a full copy of the GNU General Public License see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file handles early initialization settings for Emacs, introduced in Emacs 27.

;;; Code:

;; Show startup time after initialization
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs loaded in %s seconds with %d garbage collections."
            (emacs-init-time)
            gcs-done)))

;; Native compilation settings (available from Emacs 28)
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (message "Native compilation is available.")
  (setq
   native-comp-async-report-warnings-errors nil
   native-comp-async-jobs-number 4
   comp-deferred-compilation t))

;; Check for native JSON support and report availability
(message "Native JSON is %savailable."
         (if (functionp 'json-serialize)
             ""
           "not "))

;; Increase the maximum data read from processes to optimize performance
(setq read-process-output-max (* 1024 1024 4))

;; Disable garbage collection during initialization for faster startup
;; Re-enable it after startup when Emacs is ready
(prog1 "More generous `gc-cons-threshold' value."
  (setq garbage-collection-messages t)
  (defvar original-gc-cons-threshold gc-cons-threshold)
  (defvar better-gc-cons-threshold (* 128 12 original-gc-cons-threshold))
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (setq gc-cons-threshold better-gc-cons-threshold)
     (setq file-name-handler-alist file-name-handler-alist-original)
     (makunbound 'file-name-handler-alist-original)))

  ;; Auto-trigger garbage collection after focus change
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (if (boundp 'after-focus-change-function)
         (add-function :after after-focus-change-function
                       (lambda ()
                         (unless (frame-focus-state)
                           (garbage-collect))))
       (add-hook 'after-focus-change-function 'garbage-collect))

     ;; Setup hooks for GC during minibuffer interaction
     (defun gc-minibuffer-setup-hook ()
       (setq gc-cons-threshold most-positive-fixnum))
     (defun gc-minibuffer-exit-hook ()
       (garbage-collect)
       (setq gc-cons-threshold better-gc-cons-threshold))

     (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
     (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook))))

;; Increase recursion limits to avoid exceeding maximum recursion depth
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 10000)

;; Disable specific byte compiler warnings to reduce noise
(setq byte-compile-warnings '(not cl-functions obsolete))

;; Defer garbage collection during initialization
(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)

;; Disable package loading at startup
(setq package-enable-at-startup nil)

;; Temporarily disable file name handlers to speed up loading
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable site-run-file to avoid unnecessary startup operations
(setq site-run-file nil)

;; Configure the location of the native compilation cache (Emacs 29+)
;; This is related to the `no-littering` package and can be set here
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; UI optimizations: hide unnecessary UI elements for a cleaner experience
(setq-default
 menu-bar-mode nil
 tool-bar-mode nil
 scroll-bar-mode nil
 cursor-type 'bar
 frame-inhibit-implied-resize t
 inhibit-compacting-font-caches t
 use-dialog-box nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set preferred encoding to UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; OS and font-related settings based on the system type
(prog1 "OS- and font-related settings."
  (defvar checkos0 "Checking OS and hostname...")
  (defvar font0 "Looking for font family...")
  (defvar font1 "Setting font...")
  (defvar my-font-huckleberry "Dina:pixelsize=13"
    ;; "Hack:pixelsize=14"
    "My default font for Huckleberry.")
  (defvar my-font-family-huckleberry "Dina"
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
    ;; "Terminus:pixelsize=14"
    "Hack:pixelsize=14"
    "My default font setting for Ubuntu.")
  (defvar my-font-family-ubuntu
    ;; "Terminus"
    "Hack"
    "My default font family setting for Ubuntu.")

  (progn
    (message checkos0)
    (if (eq system-type 'gnu/linux)
        (progn
          (message (concat checkos0 "done"))
          (defvar my-os
            (substring (shell-command-to-string "lsb_release -sd") 0 -1))
          (message "Found GNU/Linux distribution: %s" my-os)
          (defvar my-hostname
            (substring (shell-command-to-string "hostname") 0 -1))
          (message "Found hostname: %s" my-hostname)

          ;; Font setup for Huckleberry
          (if (string-equal "huckleberry" (substring my-hostname 0 11))
              (progn
                (message "Current font settings for Huckleberry: %s"
                         my-font-huckleberry)
                (message font0)
                (if (and (null
                          (string=
                           "" (shell-command-to-string "which fc-list")))
                         (null
                          (string=
                           ""
                           (shell-command-to-string
                            (concat "fc-list " my-font-family-huckleberry)))))
                    (progn
                      (message (concat font0 "done"))
                      (message "Font installed: %s" my-font-family-huckleberry)
                      (add-to-list
                       'default-frame-alist `(font . ,my-font-huckleberry)))))
            ;; Font setup for Arch
            (if (string-equal "Arch" (substring my-os 1 5))
                (progn
                  (message "Current font settings for Arch Linux: %s"
                           my-font-arch)
                  (message font0)
                  (if (and (null
                            (string=
                             "" (shell-command-to-string "which fc-list")))
                           (null
                            (string=
                             ""
                             (shell-command-to-string
                              (concat "fc-list " my-font-family-arch)))))
                      (progn
                        (message (concat font0 "done"))
                        (message "Font installed: %s" my-font-family-arch)
                        (message font1)
                        (add-to-list
                         'default-frame-alist `(font . ,my-font-arch)))))
              ;; Font setup for Ubuntu
              (if (string-equal (substring my-os 0 5) (substring "Ubuntu" 0 5))
                  (progn
                    (message "Current font settings for Ubuntu: %s"
                             my-font-ubuntu)
                    (message font1)
                    (if (and (null
                              (string=
                               "" (shell-command-to-string "which fc-list")))
                             (null
                              (string=
                               ""
                               (shell-command-to-string
                                (concat "fc-list " my-font-family-ubuntu)))))
                        (progn
                          (message "Font installed: %s" my-font-family-ubuntu)
                          (message font1)
                          (add-to-list
                           'default-frame-alist `(font . ,my-font-ubuntu)))))
                (message "Adjusting frame parameters...")
                (add-to-list 'default-frame-alist '(height . 50))
                (add-to-list 'default-frame-alist '(width . 180))
                (message "Adjusting frame parameters...done"))
              (message "No predefined font settings found")))))
    (message "No Linux-based system found > font settings are not applicable")))

(provide 'early-init)
;;; early-init.el ends here
