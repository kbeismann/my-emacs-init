;;; templates.el --- summary -*- lexical-binding: t -*-

;; Author: Karsten Beismann
;; Keywords: org templates


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Some templates for org-capture.


;;; Code:

;; TODO: Clean up and restructure the templates.

(setq org-capture-templates
      ;; Basic templates for notes and URLs:
      '(
        ;; Key, name, type, target, template, options.
        ;; ("n" "Save Note" entry
        ;;  (file+headline "~/gitdir/orgdir/notes.org" "UNCATEGORIZED")
        ;;  "* TODO \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:Created: %U\n:END:\n\n%i\n\n"
        ;;  :empty-lines 1)
        ;; Templates for my personal to-do list:
        ("m" "My to-do list")
        ("mt" "TODO" entry
         (file+headline org-todo-file "To-dos")
         "* TODO \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:Created: %U\n:END:\n\n%i\n\n"
         :empty-lines 1)
        ("mn" "Save note" entry
         (file+headline org-default-notes-file "UNCATEGORIZED")
         "* UNCATEGORIZED \[\#A\] %^{Title} %^g\n:PROPERTIES:\n:Created: %U\n:END:\n\n%i\n\n"
         :empty-lines 1)
        ("mu" "Store URL" entry
         (file+headline org-default-notes-file "UNCATEGORIZED")
         "* UNCATEGORIZED \[\#A\] %^{Title} %^g\n:PROPERTIES:\n:Created: %U\n:END:\n\n:URL: %x\n\n%i\n\n"
         :empty-lines 1)
        ("me" "Edit/fix file" entry
         (file+headline org-todo-file "To-dos")
         "* TODO \[\#C\] %^{Title} %^g:code:\n:PROPERTIES:\n:Created: %U\n:LINK: %a\n:END:\n\n%i\n\n"
         :empty-lines 1)
        ("mu" "Save URL and check later" entry
         (file+headline org-todo-file "To-dos")
         "* TODO \[\#C\] %^{Title} %^g:url:\n:PROPERTIES:\n:Created: %U\n:END:\n\n:URL: %x\n\n%i\n\n"
         :empty-lines 1)
        ("mm" "Meeting minutes" entry
         (file+headline org-default-dpdhl-notes-file "UNCATEGORIZED")
         "* TODO \[\#A\] %^{Title} :meeting:minutes:%^g\nSCHEDULED: %T\n:PROPERTIES:\n:Created: %U\n:END:\n\n- *Attendees:*\n\n  + [X] Karsten Beismann\n\n- *Agenda:*\n\n  1. ...%i\n\n - *Notes:*\n\n  + ...\n\n- *Next steps:*\n\n  + ...\n\n"
         :empty-lines 1)
        ("ms" "Stand-up" entry
         (file+headline org-default-dpdhl-notes-file "UNCATEGORIZED")
         "* TODO \[\#A\] Stand-up :meeting:standup:%^g\nSCHEDULED: %T\n:PROPERTIES:\n:Created: %U\n:END:\n\n- *Progress since the last meeting:*\n\n  1. ...%i\n\n- *Outlook:*\n\n  1. ...\n\n - *Questions/collaboration:*\n\n  + ...\n\n- *Notes:*\n\n  + ...\n\n"
         :empty-lines 1)))
(setq org-todo-keywords '((sequence
                           "UNCATEGORIZED(u)"
                           "IN_PROGRESS(i)"
                           "GET_FEEDBACK(g)"
                           "BLOCKED(k)"
                           "TODO(t)"
                           "|"
                           "DONE(d)"
                           "FORWARDED(f)"
                           "CHANCELLED(c)")))
(setq org-todo-keyword-faces
      '(("UNCATEGORIZED" . org-archived)
        ("IN_PROGRESS"   . org-link)
        ("GET_FEEDBACK"  . org-warning)
        ("BLOCKED"       . org-archived)
        ("TODO"          . org-warning)
        ("FORWARDED"     . org-done)
        ("CHANCELLED"    . org-archived)))
(provide 'templates)
;;; templates.el ends here
