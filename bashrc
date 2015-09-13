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

export PATH="$HOME/.bin:/usr/local/sbin:$PATH"

source /usr/local/etc/bash_completion.d/password-store

# rust racer
export RUST_SRC_PATH=/usr/local/src/rust/src

export PS1="\w > "
