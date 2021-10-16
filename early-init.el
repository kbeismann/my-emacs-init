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
        (setq native-comp-async-jobs-number 4)
        (setq comp-deferred-compilation t))
    (message "Native complation is *not* available.")))

(prog1
    "Check if fast JSON is available."
  (if (functionp 'json-serialize)
      (message "Native JSON is available.")
    (message "Native JSON is *not* available.")))

;; Increase data read from the processess (1MB).
(setq read-process-output-max (* 1024 1024))

;; Defer GC.
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
(setq use-dialog-box t)

;; Coding system.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

(provide 'early-init)
;;; early-init.el ends here
