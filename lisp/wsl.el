;;; wsl.el --- WSL functions -*- lexical-binding: t; coding: utf-8 -*-

;;; Code:

;; Add Windows System32 to exec-path so clip.exe can be found.
(add-to-list 'exec-path "/mnt/c/WINDOWS/system32/")
;; Add PowerShell directory to exec-path for Get-Clipboard.
(add-to-list 'exec-path "/mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/")

(defun my/wsl-open-downloads-folder ()
  "Open the Windows Downloads folder in dired."
  (interactive)
  (dired "/mnt/c/Users/kbeismann/Downloads/"))

(defun my/wsl-copy-to-clipboard (text)
  "Copy TEXT to the Windows clipboard via clip.exe in WSL.
This function is intended for `interprogram-cut-function`."
  (when (stringp text)
    (let ((clip-path (executable-find "clip.exe")))
      (if clip-path
          (with-temp-buffer
            (insert text)
            ;; Ensure clip.exe is called correctly with its full path.
            (call-process-region (point-min) (point-max) clip-path nil 0 nil))
        (message
         "Error: clip.exe not found. Cannot copy to Windows clipboard.")))))

(setq interprogram-cut-function 'my/wsl-copy-to-clipboard)

(defun my/wsl-paste-from-clipboard ()
  "Paste text from the Windows clipboard via powershell.exe in WSL.
This function is intended for `interprogram-paste-function`."
  (let ((powershell-path (executable-find "powershell.exe")))
    (if powershell-path
        (let ((text (shell-command-to-string
                     (format "%s -command Get-Clipboard" powershell-path))))
          (string-trim (replace-regexp-in-string "\r" "" text)))
      (message
       "Error: powershell.exe not found. Cannot paste from Windows clipboard.")
      "")))

(setq interprogram-paste-function 'my/wsl-paste-from-clipboard)

;; When running in a terminal, disable X clipboard behavior to ensure
;; `interprogram-cut-function` and `interprogram-paste-function` are used.
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary nil)

;;; Footer:

(provide 'wsl)
