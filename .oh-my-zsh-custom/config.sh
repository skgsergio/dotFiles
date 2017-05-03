## Usage: set "ZSH_CUSTOM" to this dir and add "source $ZSH_CUSTOM/config.sh" before "source $ZSH/oh-my-zsh.sh"
# oh-my-zsh config
ZSH_THEME="gentoo-advanced"

plugins=($plugins zsh-autosuggestions zsh-syntax-highlighting)

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
bindkey '^ ' autosuggest-execute

# Other stuff
if [[ `uname -s` == "Darwin" ]]; then
    emacs_nox() {
        /Applications/Emacs.app/Contents/MacOS/Emacs -nw -- "$@"
    }
else
    emacs_nox() {
        emacs -nw -- "$@"
    }
fi

EDITOR=emacs_nox