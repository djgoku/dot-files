#!/usr/bin/env zsh
# Fresh-machine bootstrap. Requires zsh; when piping from curl use:
#   curl -fsSL https://raw.githubusercontent.com/djgoku/dot-files/main/setup.sh | zsh
#
# This script does the three things that cannot be declared in mise config,
# then hands off. Everything else lives in config/mise/.
set -euo pipefail

REPO_BRANCH="${REPO_BRANCH:-}"   # empty = default branch
REPO_DIR="$HOME/dev/github/djgoku/dot-files"

log() { echo "[INFO] $*"; }

# 1. Clone the repo.
if [[ ! -d "$REPO_DIR" ]]; then
  log "Cloning dot-files..."
  mkdir -p "${REPO_DIR:h}"
  clone_args=(git clone)
  [[ -n "$REPO_BRANCH" ]] && clone_args+=(-b "$REPO_BRANCH")
  clone_args+=(https://github.com/djgoku/dot-files.git "$REPO_DIR")
  "${clone_args[@]}"
fi
log "Current commit: $(git -C "$REPO_DIR" log --oneline -1)"

# 2. Install mise.
if [[ ! -f ~/.local/bin/mise ]]; then
  log "Installing mise..."
  curl -fsSL https://mise.run | sh
fi
~/.local/bin/mise --version

# 3. Link the machine config into place. The [dotfiles] entry that creates this
#    symlink lives inside the directory it creates, so it cannot bootstrap
#    itself -- this is the one imperative step in the whole setup.
if [[ -e ~/.config/mise && ! -L ~/.config/mise ]]; then
  log "Moving pre-existing ~/.config/mise aside..."
  mv ~/.config/mise ~/.config/mise.pre-dotfiles.$$
fi
mkdir -p ~/.config
ln -sfn "$REPO_DIR/config/mise" ~/.config/mise

log "Handing off to mise bootstrap..."
~/.local/bin/mise bootstrap --yes
