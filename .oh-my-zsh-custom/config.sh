## Usage: set "ZSH_CUSTOM" to this dir and add "source $ZSH_CUSTOM/config.sh" before "source $ZSH/oh-my-zsh.sh"
# oh-my-zsh config
ZSH_THEME="gentoo-advanced"

plugins=($plugins zsh-autosuggestions zsh-syntax-highlighting)

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
bindkey '^ ' autosuggest-execute

# Other stuff
if [[ -d "$HOME/bin" ]]; then
    export PATH="$HOME/bin:$PATH"
fi

EDITOR="emacs -nw"

alias difff="git diff --no-index"

haste() { a=$(cat); curl -X POST -s -d "$a" https://hastebin.com/documents | awk -F '"' '{print "https://hastebin.com/"$4}'; }
