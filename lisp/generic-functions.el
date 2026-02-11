;;; generic-functions.el --- Generic custom functions -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; This file contains generic custom functions that are not part of specific
;; use-package configurations.

;;; Code:

;; From https://www.emacswiki.org/emacs/UnfillParagraph.
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph.
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'my/unfill-paragraph)

;; Manual zoom.
(defun my/scale-ui (factor)
  "Scale entire UI by FACTOR."
  (interactive "nScale factor (e.g. 1.2): ")
  (let ((new-height (truncate (* factor (face-attribute 'default :height)))))
    (set-face-attribute 'default nil :height new-height)
    (set-face-attribute 'mode-line nil :height new-height)
    (set-face-attribute 'mode-line-inactive nil :height new-height)
    (set-face-attribute 'line-number nil :height new-height)
    (set-face-attribute 'fringe nil :height new-height)))
(global-set-key
 (kbd "C-+")
 (lambda ()
   (interactive)
   (my/scale-ui 1.1)))
(global-set-key
 (kbd "C-=")
 (lambda ()
   (interactive)
   (my/scale-ui 0.9)))

(defun my/copy-git-current-sha ()
  "Copy the current Git commit SHA to the clipboard."
  (interactive)
  (let ((sha (string-trim (shell-command-to-string "git rev-parse HEAD"))))
    (when (string-match-p "^[0-9a-f]\\{40\\}$" sha)
      (kill-new sha)
      (message "Copied SHA: %s" sha))))
(define-key global-map (kbd "C-c c s") 'my/copy-git-current-sha)

(defun my/copy-current-path-to-file ()
  "Copies the path of the current file to the clipboard."
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "Copied file path: %s" buffer-file-name))
    (message "No file is currently visiting.")))
(define-key global-map (kbd "C-c c p") 'my/copy-current-path-to-file)

(defun my/go-to-chezmoi-directory ()
  "Go to the Chezmoi configuration directory."
  (interactive)
  (let ((chezmoi-dir (expand-file-name "~/.local/share/chezmoi/")))
    (find-file chezmoi-dir)))
(define-key global-map (kbd "C-c c d") 'my/go-to-chezmoi-directory)

(defun my/insert-current-date-time ()
  "Insert the current date and time in a standard Emacs format."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a %H:%M>")))
(global-set-key (kbd "C-c d t i") 'my/insert-current-date-time)

(defun my/insert-current-date ()
  "Insert the current date in a standard Emacs format."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))
(global-set-key (kbd "C-c d i") 'my/insert-current-date)

(defun my/find-first-non-ascii-char ()
  "Find the first non-ASCII character from point onward."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char)) 'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
      (message "No non-ASCII characters."))))
(global-set-key (kbd "C-S-s") 'my/find-first-non-ascii-char)

(defun my/collapse-multiple-blank-lines ()
  "Collapse multiple blank lines into a single blank line in the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (replace-regexp "^[[:space:]]*\n\\(?:[[:space:]]*\n\\)+" "\n"
                      nil (point-min) (point-max)))))

(defun my/batch-collapse-blank-lines (directory)
  "Collapse multiple blank lines in all files within DIRECTORY and its subdirectories."
  (interactive "DDirectory to process: ")
  (let ((files (find-lisp-find-files directory "."))
        (processed-count 0))
    (message "Processing files recursively in %s..." directory)
    (dolist (file files)
      (condition-case err
          (progn
            (message "  Processing %s..." (file-relative-name file directory))
            (let ((buffer (find-file-noselect file)))
              (with-current-buffer buffer
                (let ((original-modified-p (buffer-modified-p)))
                  (my/collapse-multiple-blank-lines)
                  (when (buffer-modified-p)
                    (save-buffer)
                    (setq processed-count (1+ processed-count)))
                  (unless original-modified-p
                    (kill-buffer buffer))))))
        (error
         (message "Error processing %s: %s"
                  (file-relative-name file directory)
                  (error-message-string err))))
      (sit-for 0))
    (message "Finished processing files recursively in %s. %d files modified."
             directory
             processed-count)))

(defun my/update-pr ()
  "Run the update_pr command in a shell that sources ~/.bashrc."
  (interactive)
  (let ((exit-code (shell-command "bash -i -c 'update_pr'")))
    (if (= exit-code 0)
        (message "Pull request updated successfully.")
      (message "Failed to update pull request. Exit code: %d" exit-code))))
(define-key global-map (kbd "C-c u p") 'my/update-pr)

(provide 'generic-functions)
;;; generic-functions.el ends here
