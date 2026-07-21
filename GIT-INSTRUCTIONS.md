# Git instructions

Every commit in this repository must follow these standards:

- **Style**: Use sentence case for titles (Capitalize first word and proper
  nouns/acronyms only).
- **Subject**: Use the imperative mood (e.g., "Add", "Fix"). Keep it under 50
  characters and do not end with a period.
- **Body**: A body is mandatory after a single blank line. Explain the "Why" and
  "What."
- **Formatting**:
  - Wrap body text at 72 characters.
  - Do not use lists; use continuous text paragraphs.
  - Surround functions, commands, and filenames with backticks (e.g.,
    `init.el`).
- **Patterns**:
  - Use conventional commit prefixes only if consistent with history.
  - Reference previous changes using the full 40-character commit SHA.
- **Safety**: Follow the synchronization and history-rewrite policy below.

## Synchronization and history rewrites

Always run `git pull` before making changes. Immediately before every push,
fetch the remote and compare the current branch with its upstream. For an
ordinary push, pull any upstream commits that are not in the current branch,
resolve conflicts, and rerun the applicable checks. If the remote advances
during an ordinary push, repeat this synchronization instead of overwriting
remote work.

For an intentional rewrite of commits that already exist upstream, record the
upstream's full ref name and exact 40-character object ID after initial
synchronization and before changing history. Fetch again immediately before
publication, but do not pull commits that the rewrite intentionally replaces.
Continue only when the remote ref still points to the recorded object ID. If it
has changed, inspect and incorporate the new remote work into the rewrite, then
reverify every resulting commit.

Agents and automation may rewrite unpublished local history whenever useful.
They may rewrite commits that already exist upstream only as part of an explicit
user request or approval. An explicit request to amend, rebase, squash, split,
reorder, or otherwise rewrite published commits authorizes the corresponding
lease-protected publication unless the user asks to keep the rewrite local. A
general instruction to push or synchronize does not authorize choosing to
rewrite published history.

Never publish rewritten history with `--force`, `-f`, a leading `+` refspec, or
`--mirror`. Push exactly one branch with an explicit refspec and an exact lease:

```bash
git push <remote> HEAD:<full-upstream-ref> \
  --force-with-lease=<full-upstream-ref>:<expected-object-id>
```

Do not use bare `--force-with-lease` or its ref-only form because background
fetches can change the inferred expected value. Treat a rejected lease as
evidence that the remote advanced. Inspect and incorporate that work instead of
retrying with a weaker option or replacing the expected object ID without
reviewing the new remote state.
