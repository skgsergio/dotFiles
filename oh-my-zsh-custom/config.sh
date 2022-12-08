## Usage: set "ZSH_CUSTOM" to this dir and add "source $ZSH_CUSTOM/config.sh" before "source $ZSH/oh-my-zsh.sh"
# oh-my-zsh config
ZSH_THEME="gentoo-advanced"

plugins=($plugins sudo git z python golang kubectl gh)

# zsh-syntax-highlighting
plugins=($plugins zsh-syntax-highlighting)
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

# zsh-autocomplete
plugins=($plugins zsh-autocomplete)
#zstyle ':autocomplete:*' min-delay 0.5
#zstyle ':autocomplete:*' min-input 2

# Other stuff
if [[ -d "$HOME/go/bin" ]]; then
    export PATH="$HOME/go/bin:$PATH"
fi

if [[ -d "$HOME/bin" ]]; then
    export PATH="$HOME/bin:$PATH"
fi

if command -v podman &> /dev/null; then
    export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
fi

export EDITOR="emacs"

alias o="xdg-open"
alias difff="git diff --no-index"
alias ip="ip --color=auto"

azj() {
    az $@ | jq
}

haste() {
    a=$(cat)
    curl -X POST -s -d "$a" https://hastebin.com/documents | awk -F '"' '{print "https://hastebin.com/"$4}'
}

chkcert() {
    if [ "$#" -eq 2 ]; then
        chk_srv="$1"
        chk_port="$2"
        chk_sni=""
    elif [ "$#" -eq 3 ]; then
        chk_srv="$1"
        chk_port="$2"
        chk_sni="$3"
    else
        echo "Usage: $0 name_or_ip port optional_sni"
        return 1
    fi

    if [ -z "$chk_sni" ]; then
        openssl s_client -connect "${chk_srv}:${chk_port}"
    else
        openssl s_client -connect "${chk_srv}:${chk_port}" -servername "${chk_sni}"
    fi < /dev/null | openssl x509 -noout -text
}
