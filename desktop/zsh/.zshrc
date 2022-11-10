export ZSH="$XDG_DATA_HOME"/oh-my-zsh 
ZSH_THEME="ys"
source $ZSH/oh-my-zsh.sh

setopt AUTO_CONTINUE

alias curls="curl --data-binary @- curls.it"

export GPG_TTY=$TTY

# https://unix.stackexchange.com/q/home/jimothy/.zsh_history_alt
HISTSIZE=500000
SAVEHIST=500000
setopt appendhistory
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# disown after ctrlz also restarts the process
setopt AUTO_CONTINUE

[ -f "/home/jimothy/.ghcup/env" ] && source "/home/jimothy/.ghcup/env" # ghcup-env