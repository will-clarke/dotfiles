source ~/.aliases

if [ -f ~/.secrets/secrets ]
then
  source ~/.secrets/secrets
fi

source /usr/local/etc/bash_completion.d/password-store