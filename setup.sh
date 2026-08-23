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

# 3. Link the machine config by hand. This is the only unavoidable imperative
#    step: ~/.config/mise holds the very config mise must read to learn that
#    ~/.config/mise is a managed symlink.
log "Linking machine config into place..."
if [[ -e ~/.config/mise && ! -L ~/.config/mise ]]; then
  log "Moving pre-existing ~/.config/mise aside..."
  mv ~/.config/mise ~/.config/mise.pre-dotfiles.$$
fi
mkdir -p ~/.config
ln -sfn "$REPO_DIR/config/mise" ~/.config/mise

# 4. Apply ~/.dot-files on its own, before anything else. mise validates every
#    [dotfiles] source before applying any entry, and every other source is
#    written as ~/.dot-files/... -- so a full run fails up front with "sources
#    do not exist" even though the entry creating it is right there. Targeting
#    one entry validates only that entry. Going through mise rather than `ln`
#    keeps the path declared in exactly one place: the config.
log "Applying ~/.dot-files..."
~/.local/bin/mise bootstrap dotfiles apply --yes '~/.dot-files'

if (( ! bootstrap )); then
  log "--prepare-only: stopping before mise bootstrap."
  exit 0
fi

log "Handing off to mise bootstrap..."
~/.local/bin/mise bootstrap --yes
