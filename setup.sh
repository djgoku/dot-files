#!/usr/bin/env zsh
# Fresh-machine bootstrap. Requires zsh; when piping from curl use:
#   curl -fsSL https://raw.githubusercontent.com/djgoku/dot-files/main/setup.sh | zsh
#
# Usage:
#   setup.sh                    clone if needed, install mise, link, bootstrap
#   setup.sh --no-clone         use the checkout already at $REPO_DIR; fail if absent
#   setup.sh --prepare-only     stop after linking; skip `mise bootstrap`
#
# CI calls this rather than reimplementing the linking. When the two drifted,
# the workflow created a ~/.dot-files that setup.sh did not, so the cheap job
# passed while a real bootstrap failed on "sources do not exist".
set -euo pipefail

REPO_BRANCH="${REPO_BRANCH:-}"   # empty = default branch
REPO_DIR="$HOME/dev/github/djgoku/dot-files"

clone=1
bootstrap=1

# Inlined rather than sed'd out of $0: zsh rebinds $0 to the function name
# inside a function, and $0 is not a file at all when piped from curl.
usage() {
  cat <<'USAGE'
Usage:
  setup.sh                    clone if needed, install mise, link, bootstrap
  setup.sh --no-clone         use the checkout already at $REPO_DIR; fail if absent
  setup.sh --prepare-only     stop after linking; skip `mise bootstrap`
USAGE
}

for arg in "$@"; do
  case "$arg" in
    --no-clone)     clone=0 ;;
    --prepare-only) bootstrap=0 ;;
    -h|--help)      usage; exit 0 ;;
    *) echo "[ERROR] unknown option: $arg" >&2; usage >&2; exit 2 ;;
  esac
done

log() { echo "[INFO] $*"; }

# 1. Obtain the repo.
if (( clone )); then
  if [[ ! -d "$REPO_DIR" ]]; then
    log "Cloning dot-files..."
    mkdir -p "${REPO_DIR:h}"
    clone_args=(git clone)
    [[ -n "$REPO_BRANCH" ]] && clone_args+=(-b "$REPO_BRANCH")
    clone_args+=(https://github.com/djgoku/dot-files.git "$REPO_DIR")
    "${clone_args[@]}"
  fi
elif [[ ! -d "$REPO_DIR" ]]; then
  # Loud rather than silently cloning: with --no-clone the caller is asserting
  # it already put a checkout there, and a clone would test the wrong code.
  echo "[ERROR] --no-clone given but $REPO_DIR does not exist" >&2
  exit 1
fi
log "Current commit: $(git -C "$REPO_DIR" log --oneline -1)"

# 2. Install mise.
if [[ ! -f ~/.local/bin/mise ]]; then
  log "Installing mise..."
  curl -fsSL https://mise.run | sh
fi
~/.local/bin/mise --version

# 3. Let mise create its own symlinks. MISE_CONFIG_DIR points it straight at
#    the config inside the clone, so nothing here has to know where those
#    symlinks go -- the paths stay declared exactly once, in [dotfiles]. mise
#    then writes ~/.config/mise -> ~/.dot-files/config/mise, honouring
#    dotfiles.root, which a hand-rolled `ln` to the repo path would not.
#
#    One target per invocation, in this order: mise validates the sources of
#    every target named in a single call, and ~/.config/mise's source lives
#    under ~/.dot-files, which does not exist yet on the first pass.
if [[ -e ~/.config/mise && ! -L ~/.config/mise ]]; then
  log "Moving pre-existing ~/.config/mise aside..."
  mv ~/.config/mise ~/.config/mise.pre-dotfiles.$$
fi

log "Applying ~/.dot-files and ~/.config/mise..."
export MISE_CONFIG_DIR="$REPO_DIR/config/mise"
~/.local/bin/mise bootstrap dotfiles apply --yes '~/.dot-files'
~/.local/bin/mise bootstrap dotfiles apply --yes '~/.config/mise'
#    Unset so everything after this reads config through the symlink, exactly
#    as a normal invocation on this machine will.
unset MISE_CONFIG_DIR

if (( ! bootstrap )); then
  log "--prepare-only: stopping before mise bootstrap."
  exit 0
fi

log "Handing off to mise bootstrap..."
~/.local/bin/mise bootstrap --yes
