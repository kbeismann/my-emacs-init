;;; wsl.el --- WSL functions -*- lexical-binding: t; coding: utf-8 -*-

;;; Code:

;; Add Windows System32 to exec-path so clip.exe can be found.
(add-to-list 'exec-path "/mnt/c/WINDOWS/system32/")
;; Add PowerShell directory to exec-path for Get-Clipboard.
(add-to-list
 'exec-path "/mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/")

(defun my/wsl-copy-to-clipboard (text)
  "Copy TEXT to the clipboard.
Prefer `wl-copy' via WSLg.  Fall back to `clip.exe'."
  (when (stringp text)
    (let ((wl-copy (executable-find "wl-copy"))
          (clip (executable-find "clip.exe")))
      (cond
       (wl-copy
        (with-temp-buffer
          (insert text)
          (call-process-region
           (point-min) (point-max) wl-copy nil 0 nil)))
       (clip
        (with-temp-buffer
          (insert text)
          (call-process-region
           (point-min) (point-max) clip nil 0 nil)))
       (t
        (message
         "No clipboard helper found (wl-copy or clip.exe)"))))))

(defun my/wsl-paste-from-clipboard ()
  "Paste text from the clipboard.
Prefer `wl-paste' via WSLg.  Fall back to PowerShell."
  (let ((wl-paste (executable-find "wl-paste"))
        (powershell (executable-find "powershell.exe")))
    (cond
     (wl-paste
      (let ((text (shell-command-to-string wl-paste)))
        (string-trim (replace-regexp-in-string "\r" "" text))))
     (powershell
      (let ((text (shell-command-to-string
                   (format "%s -command Get-Clipboard" powershell))))
        (string-trim (replace-regexp-in-string "\r" "" text))))
     (t
      (message
       "No clipboard helper found (wl-paste or powershell.exe)")
      ""))))

;; Only override clipboard for terminal Emacs.  Under WSLg GUI Emacs
;; connects to Wayland natively (DISPLAY=:0, WAYLAND_DISPLAY=wayland-0)
;; and its built-in clipboard code already bridges to Windows.
;; Terminal Emacs needs an external helper because it has no display.
(defun my/wsl-setup-clipboard (&optional frame)
  "Configure clipboard for FRAME based on whether it is graphical.
GUI frames use native Wayland clipboard.  Terminal frames use
`wl-clipboard' or Windows executables."
  (let ((graphic (display-graphic-p (or frame (selected-frame)))))
    (cond
     (graphic
      (setq x-select-enable-clipboard t)
      (setq x-select-enable-primary t)
      (setq interprogram-cut-function nil)
      (setq interprogram-paste-function nil))
     (t
      (setq x-select-enable-clipboard nil)
      (setq x-select-enable-primary nil)
      (setq interprogram-cut-function 'my/wsl-copy-to-clipboard)
      (setq interprogram-paste-function
            'my/wsl-paste-from-clipboard)))))

(my/wsl-setup-clipboard)
(add-hook 'after-make-frame-functions #'my/wsl-setup-clipboard)

(defun my/wsl-open-downloads-folder ()
  "Open the Windows Downloads folder in dired."
  (interactive)
  (dired "/mnt/c/Users/kbeismann/Downloads/"))

(defun my/adjust-font-size-for-monitor (&optional frame)
  "Adjust font size based on the current monitor resolution."
  (interactive)
  (let* ((f (or frame (selected-frame)))
         (monitor-attrs (frame-monitor-attributes f))
         (geometry (assoc 'geometry monitor-attrs))
         (width (nth 3 geometry)))
    (when width
      ;; Width >= 3840 is the 4K screen.
      ;; Width around 1920 (or less if logical) are the small screens.
      (let ((new-height
             (if (>= width 3840)
                 150
               100)))
        (set-face-attribute 'default f
                            :height new-height
                            :family "Hack")
        (message
         "Monitor width: %d, setting font height to: %d with Hack font"
         width new-height)))))

;; Increase frequency of checks to catch moves between monitors.
(add-hook
 'after-make-frame-functions #'my/adjust-font-size-for-monitor)
(add-hook 'focus-in-hook #'my/adjust-font-size-for-monitor)
;; Use a timer to poll for monitor changes since move-frame-functions
;; are often not triggered reliably in WSLg/Wayland.
(run-with-timer 2 2 #'my/adjust-font-size-for-monitor)

;; Initial adjustment for the current frame.
(my/adjust-font-size-for-monitor)

;;; Footer:

(provide 'wsl)
