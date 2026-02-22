#!/usr/bin/env zsh
# Requires zsh - when piping from curl, use: curl -fsSL <url> | zsh
set -euo pipefail

# Configuration - can be overridden via environment variables
REPO_BRANCH="${REPO_BRANCH:-}"  # Empty = use default branch
# Local Gachix nix binary cache — override via env for other networks
NIX_CACHE_URL="${NIX_CACHE_URL:-http://10.5.0.137:8080}"
NIX_CACHE_PUBLIC_KEY="${NIX_CACHE_PUBLIC_KEY:-gachix-local-1:HBml2nUvpiVd9V48CmRl3R2EyLqB36mha/2tryd+XQ4=}"

# Logging functions
log_info() { echo "[INFO] $*"; }
log_warn() { echo "[WARN] $*" >&2; }
log_error() { echo "[ERROR] $*" >&2; }

# Helper functions
check_command() { command -v "$1" >/dev/null 2>&1; }
is_macos() { [[ "$(uname -s)" == "Darwin" ]]; }

timed() {
  local start=$SECONDS
  "$@"
  local elapsed=$(( SECONDS - start ))
  log_info "$1 completed in ${elapsed}s"
}

# Bootstrap functions
setup_directories() {
  log_info "Setting up directories..."

  if [[ ! -d ~/dev/github/djgoku ]]; then
    mkdir -p ~/dev/github/djgoku
  fi

  if [[ ! -d ~/dev/github/djgoku/dot-files ]]; then
    cd ~/dev/github/djgoku

    local -a clone_args=(git clone --depth 1)
    [[ -n "$REPO_BRANCH" ]] && clone_args+=(-b "$REPO_BRANCH")
    clone_args+=(https://github.com/djgoku/dot-files.git)

    "${clone_args[@]}"
  fi

  cd ~/dev/github/djgoku/dot-files

  log_info "Current commit: $(git log --oneline -1)"
}

install_nix() {
  log_info "Installing Nix..."

  local nix_conf
  nix_conf=$(mktemp)
  cat << EOF > "$nix_conf"
# Written by https://github.com/DeterminateSystems/nix-installer.
# The contents below are based on options specified at installation time.
extra-substituters = ${NIX_CACHE_URL}
extra-trusted-substituters = ${NIX_CACHE_URL}
extra-trusted-public-keys = ${NIX_CACHE_PUBLIC_KEY}
trusted-users = admin
EOF

  sudo mkdir -p /etc/nix
  sudo cp "$nix_conf" /etc/nix/nix.custom.conf
  rm "$nix_conf"

  if sudo launchctl list systems.determinate.nix-daemon &>/dev/null; then
    sudo launchctl kickstart -k system/systems.determinate.nix-daemon
  fi

  if [[ -d /nix ]] || check_command nix; then
    if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
      . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    fi
  else
    VERSION="v3.15.2"
    curl -fsSL https://install.determinate.systems/nix/tag/${VERSION} | sh -s -- install --no-confirm
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  fi

  nix --version
}

install_mise() {
  log_info "Installing mise..."

  if [[ ! -f ~/.local/bin/mise ]]; then
    # Installer respects MISE_VERSION env var if set, otherwise installs latest
    curl -fsSL https://mise.run | sh
  fi

  ~/.local/bin/mise --version
  ~/.local/bin/mise trust ~/dev/github/djgoku/dot-files/mise.toml
}

setup_mise_plugins() {
  log_info "Setting up mise plugins..."

  ~/.local/bin/mise plugin install nix https://github.com/jbadeau/mise-nix.git || log_info "Plugin already installed"
}

main() {
  log_info "Starting dev environment setup..."

  local total_start=$SECONDS

  timed setup_directories
  timed install_nix
  timed install_mise
  timed setup_mise_plugins

  log_info "Bootstrap complete, handing off to mise..."
  ~/.local/bin/mise run setup

  log_info "Total setup completed in $(( SECONDS - total_start ))s"
}

main "$@"
