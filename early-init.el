;;; early-init.el ---  Early initialization file -*- lexical-binding: t; coding: utf-8 -*-


;; Copyright (C) 2019 Karsten E. Beismann

;; Author: Karsten Beismann
;; Homepage: https://github.com/kbeismann/emacs-init
;; Created: Tue Sep 24 21:43:39 2019 +0200
;; Package-Requires: ((emacs "28"))
;; Keywords: init emacs early


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

;; This file is used for running early initializations before init.el.  This
;; feature was introduced with Emacs 27.


;;; Code:

(prog1 "Show startup time."
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs loaded in %s seconds with %d garbage collections."
                       (emacs-init-time "%.2f")
                       gcs-done))))

(prog1
    "Make use of the native compilation system introduced in emacs 28."
  (defconst
    have-native-compilation
    (and (fboundp 'native-comp-available-p)
         (native-comp-available-p)))
  (if have-native-compilation
      (prog1
          "Native compilation available."
        (message "Native compilation is available.")
        (setq native-comp-async-report-warnings-errors nil)
        (setq native-comp-async-jobs-number 4)
        (setq comp-deferred-compilation t))
    (message "Native complation is *not* available.")))

(prog1
    "Check if fast JSON is available."
  (if (functionp 'json-serialize)
      (message "Native JSON is available.")
    (message "Native JSON is *not* available.")))

;; Increase data read from the processess (4MB).
(setq read-process-output-max (* 1024 1024 4))


;; This block effectively disables garbage collection for the initialization
;; time and re-enables it after.  If the machine has enough RAM, at most 64MB
;; every time you start up a new Emacs, this will reduce package-initialize
;; time to about half.

(prog1 "More generous `gc-cons-threshold' value."
  (setq garbage-collection-messages t)
  (defvar better-gc-cons-threshold most-positive-fixnum)
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (setq gc-cons-threshold better-gc-cons-threshold)
     (setq file-name-handler-alist file-name-handler-alist-original)
     (makunbound 'file-name-handler-alist-original)))
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
       (setq gc-cons-threshold (* 800000 4)))
     (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
     (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook))))

;; Let's increase the max-lisp-eval-depth and max-specpdl-size to
;; prevent exceeding recursion limits.
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 10000)

;; Disable certain byte compiler warnings to cut down on the noise.
(setq byte-compile-warnings '(not cl-functions obsolete));; Defer GC.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Unset PES
(setq package-enable-at-startup nil)

;; Unset FNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Unset SRF
(setq site-run-file nil)

;; Some visual simplifications.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq scroll-bar-mode nil)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq cursor-type 'bar)

(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
(setq use-dialog-box t)

;; Coding system.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

(provide 'early-init)
;;; early-init.el ends here
