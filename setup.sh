#!/usr/bin/env zsh
set -euo pipefail

# Configuration - can be overridden via environment variables
REPO_BRANCH="${REPO_BRANCH:-}"  # Empty = use default branch

# Logging functions
log_info() { echo "[INFO] $*"; }
log_warn() { echo "[WARN] $*" >&2; }
log_error() { echo "[ERROR] $*" >&2; }

# Helper functions
check_command() { command -v "$1" >/dev/null 2>&1; }
is_macos() { [[ "$(uname -s)" == "Darwin" ]]; }

# Bootstrap functions
setup_directories() {
  log_info "Setting up directories..."
  log_info "Repository: djgoku/dot-files${REPO_BRANCH:+ (branch: $REPO_BRANCH)}"

  if [[ ! -d ~/dev/github/djgoku ]]; then
    log_info "Creating ~/dev/github/djgoku directory..."
    mkdir -p ~/dev/github/djgoku
  else
    log_info "Directory ~/dev/github/djgoku already exists"
  fi

  if [[ ! -d ~/dev/github/djgoku/dot-files ]]; then
    log_info "Cloning dot-files repository..."
    cd ~/dev/github/djgoku

    local clone_cmd="git clone --depth 1"
    [[ -n "$REPO_BRANCH" ]] && clone_cmd+=" -b ${REPO_BRANCH}"
    clone_cmd+=" https://github.com/djgoku/dot-files.git"

    eval "$clone_cmd"
  else
    log_info "dot-files repository already exists"
  fi

  log_info "Changing to dot-files directory..."
  cd ~/dev/github/djgoku/dot-files
}

install_nix() {
  log_info "Checking for Nix installation..."

  if [[ -d /nix ]] || check_command nix; then
    log_info "Nix already installed"
    if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
      . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    fi
  else
    log_info "Installing Nix..."
    curl -fsSL https://install.determinate.systems/nix | sh -s -- install --no-confirm
    log_info "Sourcing Nix daemon..."
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  fi

  log_info "Verifying Nix installation..."
  nix --version
}

install_mise() {
  log_info "Checking for mise installation..."

  if [[ -f ~/.local/bin/mise ]]; then
    log_info "mise already installed"
  else
    log_info "Installing mise..."
    curl https://mise.run | sh
  fi

  log_info "Verifying mise installation..."
  ~/.local/bin/mise --version
}

setup_mise_plugins() {
  log_info "Setting up mise plugins..."

  log_info "Adding mise-nix plugin..."
  ~/.local/bin/mise plugin install nix https://github.com/jbadeau/mise-nix.git || log_info "Plugin already installed"
}

install_tools() {
  log_info "Installing tools via mise..."

  log_info "Trusting mise configuration in dot-files..."
  ~/.local/bin/mise trust

  log_info "Running mise install..."
  ~/.local/bin/mise install

  log_info "Generating Emacs paths..."
  ~/.local/bin/mise run emacs-paths

  log_info "Verifying .env file created..."
  if [[ -f .env ]]; then
    log_info ".env file created successfully"
  else
    log_warn ".env file not found"
  fi
}

configure_macos() {
  log_info "Configuring macOS settings..."

  if ! is_macos; then
    log_info "Not running on macOS, skipping macOS configuration"
    return 0
  fi

  # Copy Terminal plist if exists
  if [[ -f com.apple.Terminal.plist ]]; then
    log_info "Copying Terminal preferences..."
    cp com.apple.Terminal.plist ~/Library/Preferences/
  else
    log_warn "com.apple.Terminal.plist not found, skipping Terminal preferences"
  fi

  # Apply macOS defaults
  log_info "Applying macOS defaults..."

  log_info "Setting Dark mode..."
  defaults write NSGlobalDomain AppleInterfaceStyle -string "Dark"

  log_info "Disabling natural scroll direction..."
  defaults write .GlobalPreferences.plist com.apple.swipescrolldirection 0

  log_info "Setting Finder to list view..."
  defaults write com.apple.finder FXPreferredSearchViewStyle Nlsv
  defaults write com.apple.finder FXPreferredViewStyle Nlsv

  log_info "Showing all files in Finder..."
  defaults write com.apple.finder AppleShowAllFiles -boolean true

  log_info "Restarting Finder..."
  killall Finder

  # Verify settings
  log_info "Verifying macOS settings..."
  log_info "Dark mode: $(defaults read NSGlobalDomain AppleInterfaceStyle 2>/dev/null || echo 'Not set')"
  log_info "Scroll direction: $(defaults read .GlobalPreferences.plist com.apple.swipescrolldirection 2>/dev/null || echo 'Not set')"
  log_info "Finder search view: $(defaults read com.apple.finder FXPreferredSearchViewStyle 2>/dev/null || echo 'Not set')"
  log_info "Finder view: $(defaults read com.apple.finder FXPreferredViewStyle 2>/dev/null || echo 'Not set')"
  log_info "Show all files: $(defaults read com.apple.finder AppleShowAllFiles 2>/dev/null || echo 'Not set')"
}

verify_setup() {
  log_info "Verifying setup..."

  log_info "Checking mise version..."
  ~/.local/bin/mise --version

  log_info "Verifying critical tools installed..."
  ~/.local/bin/mise which git
  ~/.local/bin/mise which Emacs
  ~/.local/bin/mise which node

  log_info "Testing Emacs launches..."
  $(~/.local/bin/mise which Emacs) --version

  log_info "Verifying .env file..."
  if [[ -f .env ]]; then
    log_info ".env file contents:"
    cat .env
  else
    log_warn ".env file not found"
  fi
}

setup_global() {
  log_info "Setting up global mise configuration..."

  ~/.local/bin/mise run setup-global
}

main() {
  log_info "Starting dev environment setup..."

  setup_directories
  install_nix
  install_mise
  setup_mise_plugins
  install_tools
  configure_macos
  verify_setup
  setup_global

  log_info "Setup complete! Next steps:"
  log_info "  1. Source mise: eval \"\$(~/.local/bin/mise activate zsh)\""
  log_info "  2. Test global access: mise which git"
  log_info "  3. Launch Emacs: mise run emacs"
}

main "$@"
