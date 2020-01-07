##### -*- mode:shell-script -*-

if [ -e ~/.pathrc ]; then
    source ~/.pathrc
fi

if hash rbenv &>/dev/null ; then
    eval "$(rbenv init - --no-rehash)"
fi

if hash hub &>/dev/null; then
    eval "$(hub alias -s)"
fi

if hash go &>/dev/null ; then
    export PATH=$PATH:$GOPATH/bin
    export PATH=$PATH:$(go env GOPATH)/bin
    export GOPATH=$(go env GOPATH)
fi

if [ -e /System/Library/Frameworks/JavaVM.framework/Versions/A/Commands/java_home ]; then
    export JAVA_HOME=$(/System/Library/Frameworks/JavaVM.framework/Versions/A/Commands/java_home)
fi

SHELL_SESSION_HISTORY=0
HISTFILESIZE=2000
HISTSIZE=1000

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

export SSL_CERT_FILE=/usr/local/etc/openssl/cert.pem


#### SETUP GPG AGENT
if [ $(uname) != "Darwin" ] && [ ! -S "$HOME/.gnupg/S.gpg-agent" ]; then
    eval $(gpg-agent --daemon --log-file /tmp/gpg.log --pinentry-program /usr/local/bin/pinentry-mac)
fi

export EDITOR="emacsclient -a '' -c"
export ALTERNATE_EDITOR=""

alias e="emacsclient -a '' -c"
alias glog="git log --pretty=format:'%Cred%h%Creset %<(50,trunc)%s %Cgreen(%cr) %C(bold blue)%<(16,trunc)%an%Creset %<(32,trunc)%D'"

if fortune &>/dev/null; then
    fortune computers | cowsay
fi

if [ -e /Users/wmmc/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/wmmc/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

HISTSIZE=
HISTFILESIZE=

ssh-add ~/.ssh/id_rsa &>/dev/null
if [ -e /Users/will/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/will/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
