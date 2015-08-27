source ~/.aliases

if [ -f ~/.secrets ]
then
  source ~/.secrets
fi

source /usr/local/etc/bash_completion.d/password-store

export PS1="> "
