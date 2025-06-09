;;; early-init.el --- Early initialization file -*- lexical-binding: t; coding: utf-8 -*-

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

;; Early initialization file for Emacs. This is loaded before the package system
;; and UI is set up, and is useful for disabling GUI elements and speeding up
;; startup.

;;; Code:

;; Show startup time after initialization.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs loaded in %s seconds with %d garbage collections."
            (emacs-init-time)
            gcs-done)))

;; Native compilation settings (available from Emacs 28).
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (message "Native compilation is available.")
  (setq
   native-comp-async-report-warnings-errors nil
   native-comp-async-jobs-number 4
   comp-deferred-compilation t))

;; Increase the maximum data read from processes to optimize performance.
(setq read-process-output-max (* 1024 1024 4))

;; Disable garbage collection during initialization for faster startup.
;; Re-enable it after startup when Emacs is ready.
(setq garbage-collection-messages t)
(defvar original-gc-cons-threshold gc-cons-threshold)
(defvar better-gc-cons-threshold (* 128 12 original-gc-cons-threshold))
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold better-gc-cons-threshold)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)))

;; Auto-trigger garbage collection after focus change.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (if (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function
                     (lambda ()
                       (unless (frame-focus-state)
                         (garbage-collect))))
     (add-hook 'after-focus-change-function 'garbage-collect))))

;; Increase recursion limits to avoid exceeding maximum recursion depth.
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 10000)

;; Defer garbage collection during initialization.
(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)

;; Disable package loading at startup.
(setq package-enable-at-startup nil)

;; Temporarily disable file name handlers to speed up loading.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable site-run-file to avoid unnecessary startup operations.
(setq site-run-file nil)

;; Configure the location of the native compilation cache (Emacs 29+) This is
;; related to the `no-littering` package and can be set here.
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; UI optimizations: hide unnecessary UI elements for a cleaner experience.
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

;; Set preferred encoding to UTF-8.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

(provide 'early-init)
;;; early-init.el ends here
