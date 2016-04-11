# For tramp mode:
if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  unfunction precmd
  unfunction preexec
  PS1='$ -- '
fi

if [ -f ~/.secrets ]
then
  source ~/.secrets
fi

# load rbenv if available
if which rbenv &>/dev/null ; then
  eval "$(rbenv init - --no-rehash)"
fi

# load our own completion functions
fpath=(~/.zsh/completion $fpath)

# completion
autoload -U compinit
compinit

# load custom executable functions
for function in ~/.zsh/functions/*; do
  source $function
done

# makes color constants available
autoload -U colors
colors

# history settings
setopt hist_ignore_all_dups inc_append_history
HISTFILE=~/.zhistory
HISTSIZE=4096
SAVEHIST=4096

# awesome cd movements from zshkit
setopt autocd autopushd pushdminus pushdsilent pushdtohome cdablevars
DIRSTACKSIZE=5

# Enable extended globbing
setopt extendedglob

# Allow [ or ] whereever you want
unsetopt nomatch

# # vi mode
# bindkey -v
# bindkey "^F" vi-cmd-mode
# bindkey jj vi-cmd-mode
# bindkey jk vi-cmd-mode

# handy keybindings
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^R" history-incremental-search-backward
bindkey "^P" history-search-backward
bindkey "^Y" accept-and-hold
bindkey "^N" history-search-forward
# bindkey "^N" insert-last-word
bindkey -s "^T" "^[Isudo ^[A" # "t" for "toughguy"

# aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# stty -ixon

eval "$(rbenv init - zsh --no-rehash)"

if [ -f ~/.bin/z ]
then
. ~/.bin/z
fi

function nyancat() {telnet nyancat.dakko.us}
function hiddenOn() {defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app}
function hiddenOff() {defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app}
function manp() { ps=`mktemp -t manpageXXXX`.ps ; man -t $@ > "$ps" ; open "$ps" ; }
function stopwatch(){
    date1=`date +%s`;
    while true; do
        echo -ne "$(date -jf "%s" $((`date +%s` - $date1)) +%H:%M:%S)\r";
        sleep 0.1
    done
}

fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# added by travis gem
[ -f /Users/wmmc/.travis/travis.sh ] && source /Users/wmmc/.travis/travis.sh


# # rust:
# # export DYLD_LIBRARY_PATH=/usr/local/lib/rustlib/x86_64-apple-darwin/lib
# # export RUST_SRC_PATH=/usr/local/Cellar/rust/1.0.0-beta
# # export RUST_SRC_PATH=$HOME/rust/src #should be default now..

# # use vim as the visual editor
# export VISUAL=vim
# export EDITOR=$VISUAL

# # ensure dotfiles bin directory is loaded first
# export PATH="$HOME/.bin:/usr/local/sbin:$PATH"

# if [ -f ~/.ssh/id_rsa ] && [ -f ~/.ssh/id_rsa.pub ]; then
#   export SSH_KEYS=https://github.com/
# else
#   export SSH_KEYS=NULL
# fi

# export PATH="$HOME/.bin:$PATH"
# export BUN="$HOME/.vimrc.bundles.local"
# export VL="$HOME/.vimrc.local"

# export PS1="%~ $  "

# # enable colored output from ls, etc
# export CLICOLOR=1
