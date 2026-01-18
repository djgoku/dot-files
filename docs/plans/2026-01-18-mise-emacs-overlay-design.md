# Mise-based emacs-overlay Build Workflow

## Overview

Replace devbox-based GitHub Actions workflow with mise for building emacs-overlay nightly.

## File Changes

### New: `emacs-overlay/mise.toml`

```toml
[settings]
experimental = true

[tools]
"nix" = "latest"

[tasks.update]
description = "Update flake inputs and commit if changed"
run = """
nix flake update

if git diff --quiet flake.lock; then
  echo "No changes"
  exit 0
fi

git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
git commit -m "Updated flake inputs for latest emacs-overlay" flake.lock
git pull --rebase --autostash
git push
echo "CHANGES_EXIST=true"
"""

[tasks.build]
description = "Build emacs-git-pgtk from emacs-overlay"
run = "nix build --print-build-logs"
```

### Replace: `.github/workflows/emacs-overlay.yml`

```yaml
name: Build emacs-overlay
on:
  push:
  workflow_dispatch:
    inputs:
      build-branch:
        description: 'Branch for building emacs-overlay'
        required: true
        type: string
  schedule:
    - cron: '30 8 * * *'

jobs:
  update:
    runs-on: ubuntu-latest
    if: github.repository_owner == 'djgoku'
    outputs:
      changes_exist: ${{ steps.update.outputs.changes_exist }}
    steps:
      - uses: actions/checkout@v4
        with:
          ref: ${{ inputs.build-branch || 'build-emacs-overlay' }}

      - uses: DeterminateSystems/nix-installer-action@main
      - uses: DeterminateSystems/magic-nix-cache-action@main

      - uses: jdx/mise-action@v2
        with:
          working_directory: emacs-overlay

      - name: Update flake inputs
        id: update
        working-directory: emacs-overlay
        run: |
          OUTPUT=$(mise run update)
          if echo "$OUTPUT" | grep -q "CHANGES_EXIST=true"; then
            echo "changes_exist=true" >> $GITHUB_OUTPUT
          else
            echo "changes_exist=false" >> $GITHUB_OUTPUT
          fi

  build:
    if: needs.update.outputs.changes_exist == 'true'
    needs: update
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-15]
    steps:
      - uses: actions/checkout@v4
        with:
          ref: ${{ inputs.build-branch || 'build-emacs-overlay' }}

      - uses: DeterminateSystems/nix-installer-action@main
      - uses: DeterminateSystems/magic-nix-cache-action@main

      - uses: jdx/mise-action@v2
        with:
          working_directory: emacs-overlay

      - name: Build Emacs
        working-directory: emacs-overlay
        run: mise run build
```

### Delete

- `emacs-overlay/devbox.json`
- `emacs-overlay/devbox.lock`
- `emacs-overlay/test/` directory
- `.github/workflows/earthly.yml`

## Implementation Steps

1. Create `emacs-overlay/mise.toml` with update/build tasks
2. Replace `.github/workflows/emacs-overlay.yml` with mise-based version
3. Delete devbox files and earthly.yml
4. Commit and push to `build-emacs-overlay` branch
5. Trigger workflow manually to verify

## Benefits

- Simpler tooling (mise replaces devbox)
- Automatic nix caching via magic-nix-cache-action
- Tasks work locally too (`cd emacs-overlay && mise run build`)
