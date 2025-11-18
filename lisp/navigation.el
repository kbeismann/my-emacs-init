;;; navigation.el --- Navigation configuration -*- lexical-binding: t; coding: utf-8 -*-

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

;; This file contains navigation-related configurations, including AVY for
;; efficient text navigation.

;;; Code:

(use-package
 avy
 :after base16-theme
 :bind* (("C-z" . avy-goto-char))
 :init
 (global-unset-key (kbd "S-SPC")) ; Unbind scroll down from S-SPC.
 :config (setq avy-background t) (setq avy-all-windows t)
 ;; NOT SURE IF THIS IS CORRECT: When non-nil highlight the first decision char
 ;; with avy-lead-face-0. Do this even when the char is terminating. Normally
 ;; avy-lead-face-0 is only used for the first non-terminating decision chars.
 (setq avy-highlight-first t)

 ;; Using any command makes the face attributes accessible.
 (avy-setup-default)

 ;; Face used for first non-terminating leading chars.
 (set-face-attribute 'avy-lead-face-0 nil
                     :foreground base0A-prop
                     :background base00-prop
                     :weight 'bold)

 ;; Face used for matched leading chars. Not sure what this does.
 (set-face-attribute 'avy-lead-face-1 nil
                     :foreground base09-prop
                     :background base00-prop
                     :weight 'bold)

 ;; Face used for leading chars.
 (set-face-attribute 'avy-lead-face-2 nil
                     :foreground base0C-prop
                     :background base00-prop
                     :weight 'bold)

 ;; Face used for the leading chars.
 (set-face-attribute 'avy-lead-face nil
                     :foreground base00-prop
                     :background base0E-prop
                     :weight 'bold)

 ;; Face for foreground/font during selection: base03.
 (set-face-attribute 'avy-background-face nil
                     :foreground base03-prop
                     :background base00-prop))

(use-package
 which-key
 :defer nil
 :diminish which-key-mode
 :config
 (setq which-key-idle-delay 1)
 (setq which-key-idle-secondary-delay 0)
 (which-key-mode 1))

;;; Footer:
(provide 'navigation)
;;; navigation.el ends here
