# Mise-based emacs-overlay Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace devbox with mise for nightly emacs-overlay builds in GitHub Actions.

**Architecture:** Create mise.toml with update/build tasks in emacs-overlay directory, replace GitHub workflow to use mise-action and magic-nix-cache-action, delete devbox files.

**Tech Stack:** mise, nix, GitHub Actions, DeterminateSystems nix-installer-action, magic-nix-cache-action

---

### Task 1: Create mise.toml for emacs-overlay

**Files:**
- Create: `emacs-overlay/mise.toml`

**Step 1: Create the mise.toml file**

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

**Step 2: Verify mise parses the file**

Run: `cd emacs-overlay && mise tasks`
Expected: Shows `update` and `build` tasks listed

**Step 3: Commit**

```bash
git add emacs-overlay/mise.toml
git commit -m "feat: add mise.toml for emacs-overlay builds"
```

---

### Task 2: Replace GitHub Actions workflow

**Files:**
- Replace: `.github/workflows/emacs-overlay.yml`

**Step 1: Replace the workflow file**

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

**Step 2: Validate YAML syntax**

Run: `python3 -c "import yaml; yaml.safe_load(open('.github/workflows/emacs-overlay.yml'))"`
Expected: No output (valid YAML)

**Step 3: Commit**

```bash
git add .github/workflows/emacs-overlay.yml
git commit -m "feat: replace devbox with mise in emacs-overlay workflow"
```

---

### Task 3: Delete devbox files

**Files:**
- Delete: `emacs-overlay/devbox.json`
- Delete: `emacs-overlay/devbox.lock`
- Delete: `emacs-overlay/test/devbox.json`
- Delete: `emacs-overlay/test/` directory

**Step 1: Remove devbox files and test directory**

```bash
rm emacs-overlay/devbox.json
rm emacs-overlay/devbox.lock
rm -rf emacs-overlay/test
```

**Step 2: Verify files are gone**

Run: `ls emacs-overlay/`
Expected: Only `flake.nix`, `flake.lock`, `mise.toml`

**Step 3: Commit**

```bash
git add -A emacs-overlay/
git commit -m "chore: remove devbox files from emacs-overlay"
```

---

### Task 4: Delete earthly workflow

**Files:**
- Delete: `.github/workflows/earthly.yml`

**Step 1: Remove earthly workflow**

```bash
rm .github/workflows/earthly.yml
```

**Step 2: Verify only emacs-overlay.yml remains**

Run: `ls .github/workflows/`
Expected: `emacs-overlay.yml`

**Step 3: Commit**

```bash
git add -A .github/workflows/
git commit -m "chore: remove unused earthly workflow"
```

---

### Task 5: Push and verify

**Step 1: Push to remote**

```bash
git push
```

**Step 2: Trigger workflow manually**

Go to: `https://github.com/djgoku/dot-files/actions/workflows/emacs-overlay.yml`
Click "Run workflow" with branch `build-emacs-overlay`

**Step 3: Verify workflow runs successfully**

Expected: Update job completes, build jobs run on ubuntu-latest and macos-15
