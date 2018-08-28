if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

if hash brew &>/dev/null; then
    if [ -f `brew --prefix`/etc/bash_completion ]; then
        . `brew --prefix`/etc/bash_completion
    fi
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# for linux:
if hash setxkbmap &>/dev/null; then
    setxkbmap -option 'caps:ctrl_modifier'
fi

if hash xcape &>/dev/null; then
    xcape -e 'Caps_Lock=Escape' -t 100
fi

export GPG_TTY=$(tty)

export VISUAL=emacsclient
