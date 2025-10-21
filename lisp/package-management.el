;;; package-management.el --- Package management configuration -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; This file contains the configuration for Emacs package managers, primarily
;; straight.el and use-package.

;;; Code:

(setq package-check-signature 'allow-unsigned)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities
      '(("gnu" . 2) ("org" . 1) ("melpa" . 3) ("melpa-stable" . 0)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-check-for-modifications nil)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(straight-use-package 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Disable specific byte compiler warnings to reduce noise.
(setq byte-compile-warnings '(not cl-functions obsolete))

(use-package
 auto-compile
 :init (setq load-prefer-newer t)
 :config
 (auto-compile-on-load-mode)
 (auto-compile-on-save-mode))

(use-package
 benchmark-init
 :init (benchmark-init/activate)
 :hook (after-init . benchmark-init/deactivate))

(provide 'package-management)
;;; package-management.el ends here
