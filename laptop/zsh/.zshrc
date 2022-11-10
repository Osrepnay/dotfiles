export ZSH="/home/jimtopher/.oh-my-zsh"
ZSH_THEME="ys"
source $ZSH/oh-my-zsh.sh

setopt AUTO_CONTINUE

alias curls="curl --data-binary @- curls.it"

export GPG_TTY=$TTY
[ -f "/home/jimtopher/.ghcup/env" ] && source "/home/jimtopher/.ghcup/env" # ghcup-env

# https://unix.stackexchange.com/q/home/jimtopher/.zsh_history_alt
HISTSIZE=500000
SAVEHIST=500000
setopt appendhistory
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# disown after ctrlz also restarts the process
setopt AUTO_CONTINUE
