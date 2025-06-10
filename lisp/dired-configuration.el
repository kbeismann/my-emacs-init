;;; dired-configuration.el --- Dired configuration -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; Configuration settings for Dired and related packages.

;;; Code:

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-listing-switches "-lahgF --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer nil)
(setq delete-by-moving-to-trash t)

(use-package
 dired-du
 :after dired
 :diminish dired-du-mode
 :config (setq dired-du-size-format t))

(use-package
 dired-subtree
 :after dired
 :bind
 (:map dired-mode-map (";" . dired-subtree-toggle) ("'" . dired-subtree-remove))
 :config
 (setq dired-subtree-use-backgrounds nil)
 (setq dired-subtree-line-prefix "   |-"))

(use-package
 async
 :diminish dired-async-mode
 :config
 (setq dired-async-mode 1)
 (setq async-bytecomp-package-mode 0))

(provide 'dired-configuration)
;;; dired-configuration.el ends here
