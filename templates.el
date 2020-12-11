
;; TODO: Clean up and restructure the templates.
(setq org-capture-templates

      ;; Basic templates for notes and URLs:

      '(
        ;; Key, name, type, target, template, options.
        ;; ("n" "Save Note" entry
        ;;  (file+headline "~/gitdir/orgdir/notes.org" "Unsorted")
        ;;  "* TODO \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n\n"
        ;;  :empty-lines 1
        ;;  :prepend 1)

        ("n" "Save note" entry
         (file+headline org-default-notes-file "Unsorted")
         "* UNCATEGORIZED \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n\n"
         :empty-lines 1
         :prepend 1)

        ;; Key, name, type, target, template, options.
        ("u" "Store URL" entry
         (file+headline org-default-notes-file "Unsorted")
         "* UNCATEGORIZED \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n:URL: %x\n\n%i\n\n"
         :empty-lines 1
         :prepend 1)

        ;; Templates for my personal to-do list:

        ("m" "My to-do list")

        ;; Key, name, type, target, template, options.
        ("mt" "TODO" entry
         (file+headline org-todo-file "To-dos")
         "* TODO \[\#C\] %^{Title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%i\n\n"
         :empty-lines 1
         :prepend 1)

        ;; Key, name, type, target, template, options.
        ("me" "Edit/fix file" entry
         (file+headline org-todo-file "To-dos")
         "* TODO \[\#C\] %^{Title} %^g:code:\n:PROPERTIES:\n:CREATED: %U\n:LINK: %a\n:END:\n\n%i\n\n"
         :empty-lines 1
         :prepend 1)

        ;; Key, name, type, target, template, options.
        ("mu" "Save URL and check later" entry
         (file+headline org-todo-file "To-dos")
         "* TODO \[\#C\] %^{Title} %^g:url:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n:URL: %x\n\n%i\n\n"
         :empty-lines 1
         :prepend 1)

        ;; Key, name, type, target, template, options.
        ("mm" "Meeting minutes" entry
         (file+headline org-default-dpdhl-notes-file "Unsorted")
         "* TODO \[\#A\] %^{Title} :meeting:minutes:%^g\nSCHEDULED: %T\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n- *Attendees:*\n\n  + [X] Karsten Beismann\n\n- *Agenda:*\n\n  1. ...%i\n\n - *Notes:*\n\n  + ...\n\n- *Next steps:*\n\n  + ...\n\n"
         :empty-lines 1
         :prepend 1)

        ;; Key, name, type, target, template, options.
        ("ms" "Stand-up" entry
         (file+headline org-default-dpdhl-notes-file "Unsorted")
         "* TODO \[\#A\] Stand-up :meeting:standup:%^g\nSCHEDULED: %T\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n- *Progress since the last meeting:*\n\n  1. ...%i\n\n- *Outlook:*\n\n  1. ...\n\n - *Questions/collaboration:*\n\n  + ...\n\n- *Notes:*\n\n  + ...\n\n"
         :empty-lines 1
         :prepend 1))
      org-todo-keywords ((sequence
                          "UNCATEGORIZED(u)"
                          "IN_PROGRESS(i)"
                          "GET_FEEDBACK(g)"
                          "BLOCKED(k)"
                          "TODO(t)"
                          "|"
                          "DONE(d)"
                          "FORWARDED(f)"
                          "CHANCELLED(c)")))
