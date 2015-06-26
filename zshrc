# modify the prompt to contain git branch name if applicable
git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null)
  if [[ -n $ref ]]; then
    echo " %{$fg_bold[green]%}${ref#refs/heads/}%{$reset_color%}"
  fi
}
setopt promptsubst
export PS1='${SSH_CONNECTION+"%{$fg_bold[green]%}%n@%m:"}%{$fg_bold[blue]%}%c%{$reset_color%}$(git_prompt_info) %# '

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

# enable colored output from ls, etc
export CLICOLOR=1

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

# vi mode
bindkey -v
bindkey "^F" vi-cmd-mode
bindkey jj vi-cmd-mode
bindkey jk vi-cmd-mode

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

# # extra files in ~/.zsh/configs/pre , ~/.zsh/configs , and ~/.zsh/configs/post
# # these are loaded first, second, and third, respectively.
# _load_settings() {
#   _dir="$1"
#   if [ -d "$_dir" ]; then
#     if [ -d "$_dir/pre" ]; then
#       for config in "$_dir"/pre/**/*(N-.); do
#         . $config
#       done
#     fi
#
#     for config in "$_dir"/**/*(N-.); do
#       case "$config" in
#         "$_dir"/pre/*)
#           :
#           ;;
#         "$_dir"/post/*)
#           :
#           ;;
#         *)
#           if [ -f $config ]; then
#             . $config
#           fi
#           ;;
#       esac
#     done
#
#     if [ -d "$_dir/post" ]; then
#       for config in "$_dir"/post/**/*(N-.); do
#         . $config
#       done
#     fi
#   fi
# }
# _load_settings "$HOME/.zsh/configs"
#
stty -ixon

export PATH="$HOME/.bin:$PATH"
export BUN="$HOME/.vimrc.bundles.local"
export VL="$HOME/.vimrc.local"
eval "$(rbenv init - zsh --no-rehash)"
. ~/.bin/z

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

# rust:
# export DYLD_LIBRARY_PATH=/usr/local/lib/rustlib/x86_64-apple-darwin/lib
# export RUST_SRC_PATH=/usr/local/Cellar/rust/1.0.0-beta
# export RUST_SRC_PATH=$HOME/rust/src #should be default now..
