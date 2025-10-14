((nil
  . ((eval . (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))))
 (emacs-lisp-mode
  .
  ((elisp-autofmt-empty-line-max . 1)
   (eval . (add-hook 'emacs-lisp-mode-hook #'elisp-autofmt-mode nil 'local)))))
