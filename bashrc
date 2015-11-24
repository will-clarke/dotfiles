if [ -f ~/.secrets ]
then
  source ~/.secrets
fi

if [ -f ~/.aliases ]
then
  source ~/.aliases
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi


export EDITOR='vim'
source /usr/local/etc/bash_completion.d/password-store


export PS1="\w > "
