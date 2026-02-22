# Agent Instructions for dot-files

This document provides guidance for AI agents working on this repository.

## Overview

Personal macOS development environment using [mise](https://mise.jdx.dev/) as the single tool/task manager, [Nix](https://nixos.org/) as a mise backend for system packages, and a custom Emacs build with native compilation via [elpaca](https://github.com/progfolio/elpaca).

## Architecture

```
.
├── setup.sh                # Idempotent bootstrap: nix -> mise -> mise run setup
├── mise.toml               # Tool definitions and inline tasks (see comments for why inline)
├── mise-tasks/             # File-based mise tasks
│   ├── emacs               # Launch Emacs (GUI/terminal)
│   ├── emacs-env-select    # Select dated Emacs environment
│   ├── test-emacs          # Smoke tests for CI
│   ├── setup-gpg-agent     # Generate gpg-agent.conf
│   └── setup/              # Bootstrap subtasks (clone-deps, dotfiles, install-tools, macos, texlive, treesitter)
├── emacs/
│   ├── init.el             # Emacs init (loads main.el)
│   ├── main.el             # Main configuration (~800 lines)
│   ├── early-init.el       # Early init (elpaca bootstrap)
│   ├── test-init.el        # CI smoke test harness
│   └── emacs-env/          # Dated environment selector
├── emacs-overlay/          # Custom Emacs Nix build overlay (flake.nix)
├── macos/                  # macOS Terminal.plist
├── zsh/                    # Shell configuration
├── .cirrus.yml             # CI: macOS VM bootstrap + smoke tests
└── flake.nix               # Nix flake
```

## Key Design Decisions

- **mise is the single entry point.** All tools, tasks, and environment setup flow through mise. No devbox, no standalone nix shells, no Makefiles.
- **Some tasks must stay inline in `mise.toml`** (see comments there): tasks using `{{config_root}}` template, or where a file would conflict with the `setup/` directory namespace.
- **Emacs packages are managed by elpaca**, not nix. Nix provides system-level dependencies (Emacs binary, enchant, vterm, gcc, git, gnupg).
- **`setup.sh` is the only bootstrap entry point.** It installs nix, installs mise, then hands off to `mise run setup`.

## Emacs Lisp Conventions

### Naming
- All custom functions/variables use the `johnny5-` prefix
- Internal/private helpers use `johnny5--` (double dash)

### Package Management
- Use `use-package` with elpaca for all packages
- `:ensure nil` for built-in packages
- Prefer `:custom` for `customize-set-variable` forms (VARIABLE VALUE pairs only)
- Use `:config` for imperative code (`add-hook`, `require`, `setq` for non-customizable vars)
- Use `:init` for code that must run before package loads
- Use `:after` to declare load ordering dependencies between packages

### Elisp Style
- Use built-in functions over external libraries: `string-prefix-p` not `s-starts-with-p`, `string-match-p` not `s-contains-p`
- Use `keymap-set` over deprecated `define-key` for new bindings
- Use `defcustom` (with `:type` and `:group`) for user-facing configuration variables
- Use `with-eval-after-load` to guard cross-package integration (e.g., embark + jinx)
- Defer loading where possible: `:hook`, `:bind`, `:commands`, `:after`
- Tests are inline in `main.el` using `ert-deftest` (for functions that can be tested without a running Emacs session)

### Advice
- Always document why advice is used and whether it's `:around`, `:override`, `:before`, `:after`, or `:filter-return`
- For `:override`, comment which upstream version was tested against

## Shell Script Conventions

- Shebang: `#!/usr/bin/env zsh` (this is a zsh-based environment)
- Always include `set -euo pipefail` at the top of mise task scripts
- Use `command -v` to guard optional tool sourcing (e.g., fzf, mise)
- Use `log_info`, `log_warn`, `log_error` helpers in `setup.sh`
- Idempotency: guard with `[[ ! -d ... ]]`, `check_command`, etc.
- Symlinks: use `ln -sfnv` (not `ln -sfv`) to prevent loops on re-run

## Running Tests

```bash
# Emacs config smoke test (requires mise environment)
mise run test-emacs

# Emacs ERT tests (inline in main.el, run in batch)
mise run test-emacs  # includes ert tests

# Full bootstrap test (as CI does it)
curl -fsSL setup.sh | REPO_BRANCH=main MISE_VERSION=v2026.2.10 zsh
```

## CI

Cirrus CI runs on macOS Tart VMs (`.cirrus.yml`). The pipeline:
1. Runs `setup.sh` (installs nix + mise + all tools)
2. Runs `mise run test-emacs` (Emacs smoke test)

Skipped in CI: `setup:treesitter`, `setup:texlive` (via `MISE_TASK_SKIP`).

## Common Tasks

| Command | Description |
|---------|-------------|
| `mise run emacs` | Launch Emacs (add `--nw` for terminal) |
| `mise run emacs-env-select` | Select a dated Emacs environment |
| `mise run test-emacs` | Run Emacs config smoke tests |
| `mise run setup` | Full post-bootstrap setup |
| `mise run setup-gpg-agent` | Generate gpg-agent.conf |
| `mise run generate-macos-app` | Create MiseEmacs.app in /Applications |
| `mise run install-enchant` | Install enchant spell-checking libs |
| `mise tasks` | List all available tasks |

## Commit Message Convention

```
type(scope): description

fix(emacs): move indent-bars add-hook from :custom to :config
feat(emacs): integrate Schema Store catalog with eglot
refactor(emacs): migrate config from devbox/envrc to mise
chore: remove devbox, earthly, nix/shell.nix, and pre-commit
docs: add README with setup instructions and project overview
```

Types: `feat`, `fix`, `refactor`, `chore`, `docs`, `test`
Scopes: `emacs`, `setup`, `ci`, or omit for repo-wide changes.
