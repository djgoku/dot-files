#!/bin/sh
set -euxo pipefail

# dot-files
mkdir -p ~/dev/github/djgoku
pushd ~/dev/github/djgoku
git clone --depth 1 https://github.com/djgoku/dot-files.git
pushd ~/dev/github/djgoku/dot-files

# devbox
curl -fsSL https://get.jetify.com/devbox | bash
devbox ls
eval "$(devbox global shellenv)"

# mise
mkdir -p ~/.local/bin
curl -s https://api.github.com/repos/jdx/mise/releases/latest | jq -r '.assets[] | select (.name | endswith("macos-arm64")) | .browser_download_url' | xargs -I{} sh -c 'echo \"downloading {}\n\n\" && curl -L {} > ~/.local/bin/mise'
chmod +x ~/.local/bin/mise
echo "eval \"\$(~/.local/bin/mise activate zsh)\"" >> "~/.zshrc"
eval "$(~/.local/bin/mise activate zsh)"
mise plugin install nix https://github.com/jbadeau/mise-nix.git
mise install 'nix:stow'

# stow
mkdir -p ~/.local/share/devbox/global/default
cp ~/dev/github/djgoku/dot-files/devbox/devbox.* ~/.local/share/devbox/global/default/
refresh-global
