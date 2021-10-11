export ZSH="/usr/share/oh-my-zsh/"
ZSH_THEME="ys"
source $ZSH/oh-my-zsh.sh

export PATH="$PATH:$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin"
export VK_ICD_FILENAMES=/usr/local/lib/ashesGlRenderer_icd.json
export LD_LIBRARY_PATH=/usr/local/lib
export EDITOR=nvim
export TERM=kitty
export TERMINAL=$TERM

alias curls="curl --data-binary @- curls.it"
alias vim="nvim"
alias steam="LD_PRELOAD='/usr/lib/libasound.so.2:/usr/lib64/libasound.so.2' /usr/bin/steam"
# idk either
alias dzen2="dzen2 -dock"

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
