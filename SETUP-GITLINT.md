# Agent Instructions for Setting Up Gitlint in Another Repository

These instructions guide the setup of gitlint, a Git commit message linter,
in a new repository. Gitlint enforces style standards for commit messages,
improving code quality and consistency.

## Prerequisites

- Python installed (gitlint requires Python 3.8+)
- Git repository
- Basic familiarity with pre-commit hooks (optional but recommended)

## Step-by-Step Setup

### 1. Install Pre-commit (if not already installed)

If pre-commit is not used in the repository, install it globally:

- Run: `pip install pre-commit`

Pre-commit is recommended as it integrates gitlint easily and allows running
it consistently across contributors.

### 2. Add Gitlint to Pre-commit Configuration

Edit or create `.pre-commit-config.yaml` in the repository root:

```yaml
repos:
  # ... existing repos ...

  - repo: https://github.com/jorisroovers/gitlint
    # Use the latest stable version.
    rev: v0.19.1
    hooks:
      - id: gitlint
```

For CI or manual use, you can also add:

```yaml
  - repo: https://github.com/jorisroovers/gitlint
    rev: v0.19.1
    hooks:
      - id: gitlint-ci
        stages: [manual]
```

### 3. Create Configuration File (Optional but Recommended)

Create `.gitlint` in the repository root for customizing rules:

```ini
[general]
# Ignore certain rules if not applicable
ignore=body-required,title-leading-whitespace
# Enable regex-style search for better performance
regex-style-search=True

# Define available rules by type
[rules]
# Enable verbose messages for warnings
T1=verbose

# Body rules: require at least 10 characters
B5=min-length=10

[ignore-by-author-name]
# Ignore all rules for Dependabot commits
regex=dependabot
ignore=all
```

Refer to the [Gitlint Configuration Documentation][gitlint-configuration]
for all options.

### 4. Install Pre-commit Hooks

- Run: `pre-commit install --hook-type commit-msg`
- This installs the commit-msg hook that runs gitlint on new commits.

### 5. Test the Setup

- Make a test commit with a poorly formatted message, e.g.:

  ```console
  git commit --allow-empty -m "bad commit" -m "short body"
  ```

- Should see gitlint violations reported.

### 6. Run on Existing Commits (Optional)

- To lint a specific commit: `gitlint --commit <commit-hash>`
- Or lint all commits in a range: `gitlint --commits <from>..<to>`

## Integration Options

- **CI Pipeline**: Use `gitlint-ci` hook with pre-commit run --all-files
- **Manual Check**: Run `gitlint` after commit
- **Ignore Commits**: Add `gitlint-ignore: all` to commit body to skip

## Troubleshooting

- If pre-commit complains about dependencies, ensure Python and pip are
  correctly set up.
- For rules customization, consult the [Gitlint Rules][gitlint-rules].

This setup ensures commit messages adhere to best practices, making the
repository more professional and maintainable.

[gitlint-configuration]: https://jorisroovers.github.io/gitlint/configuration/
[gitlint-rules]: https://jorisroovers.github.io/gitlint/rules/
