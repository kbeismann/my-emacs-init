# Coding conventions

Every agent or human working in this repository must adhere to the following
standards:

## Emacs Lisp

- **Formatting**: Every file modified in a commit must be formatted using the
  Emacs function `(elisp-autofmt-buffer)` before staging and committing.
- **Environment**: When running automated formatters, ensure the host's primary
  initialization flow is used (`init.el` and `.dir-locals.el`) to maintain
  stylistic consistency.
- **Integrity**: Always ensure every file ends with a single newline character.
- **Naming**: Use the `my/` naming pattern for all user-created functions and
  constants to avoid namespace collisions.
