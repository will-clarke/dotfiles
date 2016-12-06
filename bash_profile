##### -*- mode:shell-script -*-
# .bash_profile


if which rbenv &>/dev/null ; then
  eval "$(rbenv init - --no-rehash)"
fi

export PS1="\w > "

source_if_exists() {
  if [ -e "$1" ]; then
      source "$1"
  fi
}

add_to_path_if_exists() {
  path_var="$1"
  path="$2"
  if [ -e "$2" ]; then
      export "$path_var"="$path:$PATH"
  fi
}

set_if_exists() {
  var_name="$1"
  path="$2"
  if [ -e "$2" ]; then
      export "$1=$2"
  fi
}

source_if_exists "$HOME/.nix-profile/etc/profile.d/nix.sh"
source_if_exists "$HOME/.profile"
source_if_exists "$HOME/.bashrc"
source_if_exists "$HOME/.travis/travis.sh"
# Slow:
# source_if_exists "/usr/local/bin/virtualenvwrapper.sh"
# source_if_exists "/usr/local/etc/bash_completion.d/password-store"

add_to_path_if_exists "PATH" "$HOME/Library/Haskell/bin"
add_to_path_if_exists "PATH" "$HOME/dotfiles/bin"
add_to_path_if_exists "LIBRARY_PATH" "/usr/local/lib:/Users/wmmc/.nix-profile/lib"
add_to_path_if_exists "CPATH" "/usr/local/include/:/Users/wmmc/.nix-profile/include"
add_to_path_if_exists "PATH" "$HOME/.bin:/usr/local/sbin"
add_to_path_if_exists "PATH" "/Applications/Postgres.app/Contents/Versions/latest/bin"
add_to_path_if_exists "PATH" "$HOME/.cargo/bin"
set_if_exists "EDITOR" "/Users/wmmc/.bin/edit"
set_if_exists "RUST_SRC_PATH" "/usr/local/src/rust/src"
