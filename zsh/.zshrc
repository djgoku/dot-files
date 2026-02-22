# fixes vterm + zsh have trailing %
unsetopt PROMPT_SP

function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_cmd() {
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]51;E"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]51;E"
    else
        printf "\e]51;E"
    fi
    while [ $# -gt 0 ]; do
        printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')"
        shift
    done
    if [ -n "$TMUX" ]; then
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\007\e\\"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\007\e\\"
    else
        printf "\e\\"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

vterm_set_directory() {
    vterm_cmd update-pwd "$PWD/"
}

autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ vterm_set_directory }

# Directory stack (pushd/popd as cd)
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
setopt PUSHD_TO_HOME

# Case-insensitive globbing and completion
autoload -U compinit; compinit
autoload -U select-word-style
select-word-style bash

# History
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

# Completion UI - https://github.com/Aloxaf/fzf-tab.git
[[ -f ~/dev/github/Aloxaf/fzf-tab/fzf-tab.plugin.zsh ]] && \
  source ~/dev/github/Aloxaf/fzf-tab/fzf-tab.plugin.zsh

export PATH="${AQUA_ROOT_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/aquaproj-aqua}/bin:$PATH"
[[ -f ~/.local/bin/mise ]] && eval "$(~/.local/bin/mise activate zsh)"

# terminal emacs
ulimit -n 10240

command -v fzf >/dev/null 2>&1 && source <(fzf --zsh)
