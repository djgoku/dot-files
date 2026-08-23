# Agent Instructions for dot-files

This document provides guidance for AI agents working on this repository.

## Overview

Personal macOS development environment. [mise](https://mise.jdx.dev/) is the single
entry point: it installs every tool, applies dotfiles, and configures the machine
via `mise bootstrap`. Emacs is installed as an aqua package
(`aqua:djgoku/misemacs-emacs-master`) with packages managed by
[elpaca](https://github.com/progfolio/elpaca).

**Nix has been removed.** So have all mise tasks and the project-level `mise.toml`.
If you find a reference to `nix`, `mise run <task>`, `mise-tasks/`, `setup.sh`
helper functions, or `MiseEmacs.app`, it is stale — remove it rather than
reviving it.

## Architecture

```
.
├── setup.sh                    # Fresh-machine bootstrap (3 steps, then hands off)
├── config/mise/                # Machine config -> symlinked to ~/.config/mise
│   ├── config.toml             #   [settings] [tools] [env] [dotfiles]
│   └── conf.d/
│       ├── 10-repos.toml       #   git clones (fzf-tab, tree-sitter-module)
│       ├── 20-files.toml       #   managed directories and generated files
│       ├── 30-macos.toml       #   Finder and NSGlobalDomain defaults
│       └── 90-hooks.toml       #   post-repos, post-tools
├── emacs/
│   ├── init.el                 # Emacs init (loads main.el)
│   ├── main.el                 # Main configuration
│   ├── early-init.el           # Early init (elpaca bootstrap)
│   └── emacs-env/              # Dated environment selector
├── gnupg/gpg-agent.conf.tmpl   # Rendered to ~/.gnupg/gpg-agent.conf
├── macos/com.apple.Terminal.plist
├── zsh/.zshrc
├── mise.toml                   # PROJECT config: this repo's tasks only
└── .github/
    ├── PklProject              # pins com.github.actions (pkl-pantry)
    └── workflows/*.pkl         # workflow SOURCES; the .yml are generated
```

## Key Design Decisions

- **Two configs, two jobs.** `mise.toml` at the root is *project* config: this
  repo's tasks and the tools they need. `config/mise/` is *machine* config,
  reachable only through the `~/.config/mise` symlink. The directory is
  deliberately *not* named `mise/`, `.mise/`, or `.config/mise/` — mise
  auto-discovers all three as project config, which would make `[bootstrap]`
  stanzas (including `macos.defaults`) go live merely by `cd`-ing into the repo.
- **`setup.sh` holds only what cannot be declared,** and CI calls it rather than
  reimplementing it. `--no-clone` uses an existing checkout (and fails loudly if
  absent, instead of cloning main and testing the wrong code); `--prepare-only`
  stops before the expensive `mise bootstrap`. Only two things are genuinely
  imperative: cloning, and installing mise.
- **No path is symlinked by hand.** `MISE_CONFIG_DIR` points mise at the config
  inside the clone, so it creates `~/.dot-files` and `~/.config/mise` from its
  own `[dotfiles]` entries — which also means `~/.config/mise` correctly points
  through `dotfiles.root` rather than at the repo path. They are applied one
  target per invocation and in that order: mise validates the sources of every
  target named in a single call, and `~/.config/mise`'s source lives under
  `~/.dot-files`. The env var is unset afterwards so the rest of the run reads
  config through the symlink, like any normal invocation.
- **Stage order matters.** `mise bootstrap` runs 17 stages and `tools` is #15.
  Anything shelling out to an installed binary belongs in `post-tools` or
  `final`. Putting it earlier is the bug that required a `|| true` guard on the
  old `post-dotfiles` gpg hook, which then silently no-opped on fresh machines.
- **`bootstrap.files` does not create parents** and does not `mkdir -p`. Declare
  each directory level explicitly, parent first. Directories apply as part of the
  `files` stage; there is no `--only directories`.
- **Emacs packages are managed by elpaca**, not by mise.

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
- Always include `set -euo pipefail`
- Hook bodies in `conf.d/90-hooks.toml` run under `sh`, not zsh — use `set -eu`
  and POSIX constructs there
- Use `command -v` to guard optional tool sourcing (e.g. fzf, mise)
- Idempotency: guard expensive work on its output (e.g. tree-sitter grammar
  compilation is guarded on `dist/`), not on a flag
- Symlinks: use `ln -sfn` (not `ln -sf`) to prevent loops on re-run

## Running Tests

```bash
mise run check-workflows      # generated YAML still matches its .pkl source
mise bootstrap --dry-run      # inspect every stage, change nothing
mise bootstrap --only files --yes
mise config ls                # confirm which config files are actually loaded
```

A second `mise bootstrap --yes` must be a no-op; non-convergence is a bug.

## CI

GitHub Actions, in two tiers. **The workflow YAML is generated from Pkl — never
hand-edit a `.yml` under `.github/workflows/`.** Edit the `.pkl` beside it and
run `mise run render-workflows`; `mise run check-workflows` (and the `validate`
job) fails on drift.

- `validate.pkl` — per-PR, cheap. Installs nothing: asserts all five config
  files load, every bootstrap stage resolves under `--dry-run`, and the
  generated YAML matches its source.
- `bootstrap.pkl` — weekly and on demand, expensive. A real `setup.sh` run on a
  clean macOS runner, then artifact assertions and an idempotency re-run.

`.github/PklProject` pins `com.github.actions`; after changing it run
`pkl project resolve .github/` and commit `PklProject.deps.json`. Note that pkl
does *not* find a `PklProject` in an ancestor directory — the render task passes
`--project-dir .github/` for exactly this reason.

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
