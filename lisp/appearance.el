;;; appearance.el --- Appearance configuration -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; Configuration settings for Emacs's visual appearance, fonts, and display
;; enhancements. Colors intentionally follow Emacs and package defaults rather
;; than a configured theme.

;;; Code:

(setq line-spacing nil)
(setq truncate-lines t)
(setq font-lock-maximum-decoration t)
(setq diff-font-lock-syntax t)
(setq fringe-mode 1)

(setq display-line-numbers nil)
(setq display-line-numbers-width 4)
(setq display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'yaml-mode-hook #'display-line-numbers-mode)

(column-number-mode 1)
(line-number-mode 1)

(setq blink-cursor-mode t)
(setq-default cursor-type 'hollow)

;; Simplify the cursor position: No proportional position (percentage) nor texts
;; like "Bot", "Top" or "All". Source:
;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/
(setq mode-line-position
      '((line-number-mode ("%l" (column-number-mode ":%c")))))

(use-package hl-line :init (global-hl-line-mode 1))

;; Font settings
(defvar my-font "Hack-18"
  "My default font.")

(set-face-attribute 'default nil :font my-font)
(add-to-list 'default-frame-alist `(font . ,my-font))

(custom-set-faces
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-builtin-face ((t (:weight bold)))))

(provide 'appearance)
;;; appearance.el ends here
