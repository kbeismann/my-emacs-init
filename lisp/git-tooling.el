;;; git-tooling.el --- Git-related configurations -*- lexical-binding: t; coding: utf-8 -*-

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

;; Git-related configurations and tooling.

;;; Code:

;; Git tooling.
:config
;; Fixes temporary issues with vc-mode.
(setq vc-handled-backends ())

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
 :config (setq gac-automatically-push-p t)
 (setq safe-local-variable-values
       (append safe-local-variable-values '((eval (git-auto-commit-mode 1))))))

;; Allow the global eval forms in .dir-locals.el. This block must be loaded
;; early enough for .dir-locals.el to be processed correctly.
(setq safe-local-variable-values
      (append
       safe-local-variable-values
       '((eval .
               (progn
                 (git-auto-commit-mode 1)
                 (add-hook 'before-save-hook #'delete-trailing-whitespace
                           nil
                           t))))))

;;; Footer:
(provide 'git-tooling)
;;; git-tooling.el ends here
