#!/bin/sh

fancy_echo() {
  local fmt="$1"; shift

  # shellcheck disable=SC2059
  printf "\n$fmt\n" "$@"
}

trap 'ret=$?; test $ret -ne 0 && printf "failed\n\n" >&2; exit $ret' EXIT

set -e


# shellcheck disable=SC2016

case "$SHELL" in
  */zsh) : ;;
  *)
    fancy_echo "Changing your shell to zsh ..."
      chsh -s "$(which zsh)"
    ;;
esac

brew_install_or_upgrade() {
  if brew_is_installed "$1"; then
    if brew_is_upgradable "$1"; then
      fancy_echo "Upgrading %s ..." "$1"
      brew upgrade "$@"
    else
      fancy_echo "Already using the latest version of %s. Skipping ..." "$1"
    fi
  else
    fancy_echo "Installing %s ..." "$1"
    brew install "$@"
  fi
}

brew_is_installed() {
  local name="$(brew_expand_alias "$1")"

  brew list -1 | grep -Fqx "$name"
}

brew_is_upgradable() {
  local name="$(brew_expand_alias "$1")"

  ! brew outdated --quiet "$name" >/dev/null
}

brew_tap() {
  brew tap "$1" 2> /dev/null
}

brew_expand_alias() {
  brew info "$1" 2>/dev/null | head -1 | awk '{gsub(/:/, ""); print $1}'
}

brew_launchctl_restart() {
  local name="$(brew_expand_alias "$1")"
  local domain="homebrew.mxcl.$name"
  local plist="$domain.plist"

  fancy_echo "Restarting %s ..." "$1"
  mkdir -p "$HOME/Library/LaunchAgents"
  ln -sfv "/usr/local/opt/$name/$plist" "$HOME/Library/LaunchAgents"

  if launchctl list | grep -Fq "$domain"; then
    launchctl unload "$HOME/Library/LaunchAgents/$plist" >/dev/null
  fi
  launchctl load "$HOME/Library/LaunchAgents/$plist" >/dev/null
}

gem_install_or_update() {
  if gem list "$1" --installed > /dev/null; then
    fancy_echo "Updating %s ..." "$1"
    gem update "$@"
  else
    fancy_echo "Installing %s ..." "$1"
    gem install "$@"
    rbenv rehash
  fi
}

if ! command -v brew >/dev/null; then
  fancy_echo "Installing Homebrew ..."
    curl -fsS \
      'https://raw.githubusercontent.com/Homebrew/install/master/install' | ruby


    # shellcheck disable=SC2016

    export PATH="/usr/local/bin:$PATH"
else
  fancy_echo "Homebrew already installed. Skipping ..."
fi

fancy_echo "Updating Homebrew formulas ..."
brew update

brew_install_or_upgrade 'zsh'
brew_install_or_upgrade 'git'
brew_install_or_upgrade 'postgres'
brew_launchctl_restart 'postgresql'
brew_install_or_upgrade 'redis'
brew_launchctl_restart 'redis'
brew_install_or_upgrade 'the_silver_searcher'
brew_install_or_upgrade 'vim'
brew_install_or_upgrade 'ctags'
brew_install_or_upgrade 'tmux'
brew_install_or_upgrade 'reattach-to-user-namespace'
brew_install_or_upgrade 'imagemagick'
brew_install_or_upgrade 'qt'
brew_install_or_upgrade 'hub'
brew_install_or_upgrade 'node'

brew_install_or_upgrade 'rbenv'
brew_install_or_upgrade 'ruby-build'

# shellcheck disable=SC2016

brew_install_or_upgrade 'openssl'
brew unlink openssl && brew link openssl --force
brew_install_or_upgrade 'libyaml'

ruby_version="$(curl -sSL http://ruby.thoughtbot.com/latest)"

eval "$(rbenv init - zsh)"

if ! rbenv versions | grep -Fq "$ruby_version"; then
  rbenv install -s "$ruby_version"
fi

rbenv global "$ruby_version"
rbenv shell "$ruby_version"

gem update --system

gem_install_or_update 'bundler'

fancy_echo "Configuring Bundler ..."
  number_of_cores=$(sysctl -n hw.ncpu)
  bundle config --global jobs $((number_of_cores - 1))

brew_install_or_upgrade 'heroku-toolbelt'

# me
brew_install_or_upgrade 'caskroom/cask/brew-cask'
brew_install_or_upgrade 'gpg'

cask() {
  brew cask install "$1"
}

cask 'google-chrome'
cask '1password'
cask 'flux'
cask 'caffeine'
cask 'alfred'
cask 'dropbox'
cask 'bettertouchtool'
cask 'dropbox'
cask 'karabiner'
cask 'growlnotify'
cask 'utorrent'
cask 'steam'
cask 'iterm2'
cask 'firefox'
cask 'vlc'
cask 'disk-inventory-x'

brew cleanup
brew cask cleanup

fancy_echo 'Almost there.. now just installing neobundle for neovim'

BUNDLE_DIR=~/.nvim/bundle
INSTALL_DIR=$BUNDLE_DIR/neobundle.vim

if [ -e $INSTALL_DIR ]; then
  echo "$INSTALL_DIR already exists! Congrats!!!"
  exit 1
fi

# make bundle dir and fetch neobundle
echo "Begin fetching NeoBundle..."
mkdir -p ~/nvim/bundle
git clone https://github.com/Shougo/neobundle.vim ~/nvim/bundle/neobundle.vim

echo "Installing spacemacs"
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-spacemacs-icon
brew linkapps emacs-mac
git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d

# echo "make sure emacs is up to date"
# cd ~/emacs.d
# git pull --rebase
# git submodule sync; git submodule update
# cd ~

if [[ -f ~/.ssh/id_rsa && -f ~/.ssh/id_rsa.pub]]
export SSH_KEYS=true
fi

brew install homebrew/emacs/gnugo-emacs

brew install offlineimap
brew install gnutls
