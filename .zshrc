export ZSH="/home/jimothy/.oh-my-zsh"
ZSH_THEME="ys"
source $ZSH/oh-my-zsh.sh

alias curls="curl --data-binary @- curls.it"
alias ghci="cd /home/jimothy/Documents/ghci-config && stack ghci"

export GPG_TTY=$TTY
