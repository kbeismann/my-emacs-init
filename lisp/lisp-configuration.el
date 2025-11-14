;;; lisp-configuration.el --- Configuration for Lisp and Emacs Lisp modes

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

;; Configuration for Lisp and Emacs Lisp modes.

;;; Code:

(use-package
 elisp-autofmt
 :straight (:host codeberg :repo "ideasman42/emacs-elisp-autofmt")
 :commands (elisp-autofmt-mode elisp-autofmt-buffer))

;; Add the hook function to emacs-lisp-mode-hook.
(add-hook 'emacs-lisp-mode-hook #'my/add-collapse-to-before-save)

;;; Footer:
(provide 'lisp-configuration)
;;; lisp-configuration.el ends here
