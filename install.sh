#!/bin/sh

set -e

fancy_echo() {
    local fmt="$1"; shift
    printf "\n$fmt\n" "$@"
}

install_homebrew() {
    if ! command -v brew >/dev/null; then
        fancy_echo "Installing Homebrew ..."
        curl -fsS \
             'https://raw.githubusercontent.com/Homebrew/install/master/install' | ruby
        export PATH="/usr/local/bin:$PATH"
    else
        fancy_echo "You've already got Homebrew..."
    fi
}

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

ask_to_install() {
    fancy_echo "Do you want to install $1?
[yes/no]: "
    read resp
    if [ "$resp" = "y" ] || [ "$resp" = "yes" ]
    then
        install_="install_"
        eval "$install_$1"
    else
        echo "Skipping $1..."
    fi
}

install_essentials() {
    brew_install_or_upgrade 'git'
    brew_install_or_upgrade 'hub'
    git clone "https://github.com/wmmc/dotfiles"
    cd dotfiles
    rake install
    cd $HOME
}

install_db() {
    brew_install_or_upgrade 'postgres'
    brew_install_or_upgrade 'redis'
}

install_unix_stuff() {
    brew_install_or_upgrade 'the_silver_searcher'
    brew_install_or_upgrade 'heroku-toolbelt'
    brew_install_or_upgrade 'tmux'
    brew_install_or_upgrade 'gpg'
    brew_install_or_upgrade 'pass'
    brew_install_or_upgrade 'tree'
    brew_install_or_upgrade 'openssl'
    brew unlink openssl && brew link openssl --force
}

install_vim() {
    brew_install_or_upgrade 'vim'
    brew_install_or_upgrade 'ctags'
    brew_install_or_upgrade 'reattach-to-user-namespace'
}

install_javascript_stuff() {
    brew_install_or_upgrade 'imagemagick'
    brew_install_or_upgrade 'qt'
    brew_install_or_upgrade 'node'
}

install_ruby() {
    brew_install_or_upgrade 'rbenv'
    brew_install_or_upgrade 'ruby-build'
    echo 'eval "$(rbenv init -)"' >> ~/.bash_profile

    ruby_version="$(curl -sSL http://ruby.thoughtbot.com/latest)"
    fancy_echo "Do you want to update Ruby to version $ruby_version
    [yes/no]"
    read resp
    if [ "$resp" = "y" ] || [ "$resp" = "yes" ]; then
        fancy_echo "Updating ruby versions"
        update_ruby_version
    else
        fancy_echo "Okay. We won't update Ruby"
    fi
    update_gems
}

update_gems() {
    gem update --system
    gem_install_or_update 'bundler'
    fancy_echo "Configuring Bundler ..."
    number_of_cores=$(sysctl -n hw.ncpu)
    bundle config --global jobs $((number_of_cores - 1))
}

update_ruby_version() {
    ruby_version="$(curl -sSL http://ruby.thoughtbot.com/latest)"
    if ! rbenv versions | grep -Fq "$ruby_version"; then
        rbenv install -s "$ruby_version"
    fi
    rbenv global "$ruby_version"
    rbenv shell "$ruby_version"
}

install_scala() {
    brew_install_or_upgrade 'typesafe-activator'
}

cask() {
    brew cask install "$1"
}

install_comfy_setup() {
    brew_install_or_upgrade 'caskroom/cask/brew-cask'
    cask 'postgres'
    cask 'dash'
    cask 'imageoptim'
    cask '1password'
    cask 'flux'
    cask 'caffeine'
    cask 'alfred'
    cask 'dropbox'
    cask 'bettertouchtool'
    cask 'karabiner'
    cask 'growlnotify'
    cask 'iterm2'
}

install_totally_pointless_stuff() {
    brew_install_or_upgrade 'caskroom/cask/brew-cask'
    cask 'utorrent'
    cask 'steam'
    cask 'firefox'
    cask 'vlc'
    cask 'disk-inventory-x'
}


install_emacs() {
    brew tap railwaycat/emacsmacport
    brew install emacs-mac --with-spacemacs-icon
    brew linkapps emacs-mac
    git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
}

install_emacs_extensions() {
    brew_install_or_upgrade 'homebrew/emacs/gnugo-emacs'
    brew_install_or_upgrade 'libyaml'
    # brew_install_or_upgrade 'gnutls'
    brew_install_or_upgrade 'isync'
    brew_install_or_upgrade 'w3m'
    brew_install_or_upgrade 'msmtp'
}

install_everything() {
    install_essentials
    install_db
    install_unix_stuff
    install_vim
    install_javascript_stuff
    install_ruby
    install_scala
    install_comfy_setup
    install_totally_pointless_stuff
    install_emacs
    install_emacs_extensions
}

intro() {
    fancy_echo "Hey!
You're pretty chilled. Nice work. I can see you've been working out...

First question:
}

should_we_install_everything() {
    fancy_echo "Should we just install everything else?
[yes/no]"
    read resp
    if [ "$resp" = "y" ] || [ "$resp" = "yes" ]; then
        fancy_echo "Okay. We'll update it all!"
        install_everything
    else
        fancy_echo "Okay. Choose what you want:"
    fi
}

cd $HOME
intro
should_we_install_everything
install_essentials
ask_to_install "db"
ask_to_install "unix_stuff"
ask_to_install "vim"
ask_to_install "javascript_stuff"
ask_to_install "ruby"
ask_to_install "scala"
ask_to_install "comfy_setup"
ask_to_install "totally_pointless_stuff"
ask_to_install "emacs"
ask_to_install "emacs_extensions"

intro
fancy_echo "Updating Homebrew formulas ..."
brew update

brew cleanup
brew cask cleanup
