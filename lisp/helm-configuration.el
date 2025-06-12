;;; helm-configuration.el --- Helm configuration -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:

;; Configuration settings for Helm and related packages.

;;; Code:

(use-package
 image-dired
 ;; Prevent `image-dired` from being autoloaded by Helm or other packages (e.g.,
 ;; when browsing images with helm-find-files and native image preview). Emacs
 ;; 27+ uses native image-mode rendering, and loading image-dired adds
 ;; unnecessary delay (~450ms+). Providing the feature here fakes it as loaded,
 ;; so it won't be triggered via autoload or :require from other packages. NOTE:
 ;; This also prevents image-dired submodules (tags, external) from loading.
 :init
 (eval-when-compile
   (provide 'image-dired))
 ;; Unbind image-dired to ensure it's not loaded.
 (fmakunbound 'image-dired))

(use-package
 helm
 :defer nil
 :diminish (helm-mode helm-autoresize-mode helm-minibuffer-history-mode)
 :requires helm-autoloads
 :bind*
 (("M-x" . helm-M-x)
  ("C-s" . helm-occur)
  ("C-x b" . helm-mini)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-c h" . helm-command-prefix)
  ("C-c t h" . helm-tramp)
  (:map
   helm-command-map
   ("l" . helm-locate)
   ("s" . helm-surfraw)
   ("r" . helm-regexp)
   ("m" . helm-multi-files)
   ("a" . helm-apropos)
   ("i" . helm-imenu)))
 :init
 (global-unset-key (kbd "C-x c"))
 :config
 (setq helm-split-window-inside-p nil)
 (setq helm-move-to-line-cycle-in-source nil)
 (setq helm-autoresize-mode t)
 (setq helm-mode-fuzzy-match t)
 (setq helm-completion-in-region-fuzzy-match t)
 (setq helm-display-buffer-reuse-frame nil)
 (setq helm-use-undecorated-frame-option t)
 (setq helm-tramp-control-master t)
 (setq helm-grep-ag-command "ag --line-numbers -S -i --color --nogroup %s -- %s %s")
 (helm-mode 1))

(use-package helm-tramp :after helm tramp)

(use-package helm-ag :after helm)

(use-package
 helm-flyspell
 :after
 helm
 flyspell
 :bind (("C-c f c" . helm-flyspell-correct)))

(use-package
 helm-projectile
 :defer nil
 :after (projectile helm)
 :config (helm-projectile-on))

(use-package helm-lsp :after (helm lsp) :commands helm-lsp-workspace-symbol)

(provide 'helm-configuration)
;;; helm-configuration.el ends here
