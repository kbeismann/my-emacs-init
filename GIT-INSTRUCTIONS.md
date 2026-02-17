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
- **Safety**: Always run `git pull` before making changes. Do never force push.
