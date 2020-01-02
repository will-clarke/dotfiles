if [[ $TMUX_PANE ]]; then
    HISTFILE=$HOME/.bash_history_tmux_${TMUX_PANE:1}
fi

if [[ -e ~/.pathrc ]]; then
    . ~/.pathrc
fi
