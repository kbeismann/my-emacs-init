;;; appearance.el --- Appearance configuration -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; Configuration settings for Emacs's visual appearance, themes, fonts, and
;; display enhancements.

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

;; Simplify the cursor position: No proportional position (percentage) nor texts
;; like "Bot", "Top" or "All". Source:
;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/
(setq mode-line-position
      '((line-number-mode ("%l" (column-number-mode ":%c")))))

(use-package hl-line :init (global-hl-line-mode 1))

(prog1 "OS- and font-related settings."
  (defvar checkos0 "Checking OS and hostname...")
  (defvar font0 "Looking for font family...")
  (defvar font1 "Setting font...")
  (defvar my-font-huckleberry "Dina:pixelsize=13"
    "My default font for Huckleberry.")
  (defvar my-font-family-huckleberry "Dina"
    "My default font family for Huckleberry.")
  (defvar my-font-arch "Dina:pixelsize=13"
    "My default font for Arch Linux.")
  (defvar my-font-family-arch "Dina"
    "My default font family for Arch Linux.")
  (defvar my-font-ubuntu "Hack:pixelsize=14"
    "My default font setting for Ubuntu.")
  (defvar my-font-family-ubuntu "Hack"
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

          ;; Font setup for Huckleberry.
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
            ;; Font setup for Arch.
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
              ;; Font setup for Ubuntu.
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

(use-package
 base16-theme
 :defer nil
 :config (load-theme 'base16-zenburn t)

 (setq base16-theme-256-color-source "colors")

 (defvar base00-prop (nth 01 base16-zenburn-theme-colors))
 (defvar base01-prop (nth 03 base16-zenburn-theme-colors))
 (defvar base02-prop (nth 05 base16-zenburn-theme-colors))
 (defvar base03-prop (nth 07 base16-zenburn-theme-colors))
 (defvar base04-prop (nth 09 base16-zenburn-theme-colors))
 (defvar base05-prop (nth 11 base16-zenburn-theme-colors))
 (defvar base06-prop (nth 13 base16-zenburn-theme-colors))
 (defvar base07-prop ; White.
   (nth 15 base16-zenburn-theme-colors))
 (defvar base08-prop ; Pink.
   (nth 17 base16-zenburn-theme-colors))
 (defvar base09-prop ; Orange.
   (nth 19 base16-zenburn-theme-colors))
 (defvar base0A-prop ; Yellow.
   (nth 21 base16-zenburn-theme-colors))
 (defvar base0B-prop ; Green.
   (nth 23 base16-zenburn-theme-colors))
 (defvar base0C-prop (nth 25 base16-zenburn-theme-colors))
 (defvar base0D-prop (nth 27 base16-zenburn-theme-colors))
 (defvar base0E-prop (nth 29 base16-zenburn-theme-colors))
 (defvar base0F-prop (nth 31 base16-zenburn-theme-colors))

 ;; Remove the vertical line between windows.
 (set-face-background 'vertical-border base00-prop)
 (set-face-foreground 'vertical-border (face-background 'vertical-border))

 ;; Adjust mode line colors.
 (set-face-background 'mode-line base02-prop)
 (set-face-foreground 'mode-line base04-prop)
 (set-face-background 'mode-line-inactive base01-prop)
 (set-face-foreground 'mode-line-inactive base04-prop)

 ;; Hide the fringe but show linebreak arrows.
 (set-face-attribute 'fringe nil
                     :background base00-prop
                     :foreground base02-prop)

 ;; Look of the current line number. Here, the background is the color
 ;; of the number.
 (set-face-attribute 'line-number-current-line nil
                     :foreground base08-prop
                     :background base01-prop)

 ;; Look and color of the line numbers.
 (set-face-attribute 'line-number nil
                     :background base00-prop
                     :foreground base02-prop)

 (custom-set-faces
  '(font-lock-keyword-face ((t (:weight bold))))
  '(font-lock-builtin-face ((t (:weight bold))))))

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

(provide 'appearance)
;;; appearance.el ends here
