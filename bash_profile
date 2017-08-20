##### -*- mode:shell-script -*-

if hash rbenv &>/dev/null ; then
    eval "$(rbenv init - --no-rehash)"
fi

if hash hub &>/dev/null; then
    eval "$(hub alias -s)"
fi

export "PS1"="\w > "

source_file_if_exists() {
    if [ -e "$1" ]; then
        source "$1"
    fi
}

add_to_path_if_file_exists() {
    path_var="$1"
    path="$2"
    if [ -e "$2" ]; then
        export "$path_var"="$path:$PATH"
    fi
}

set_if_file_exists() {
    var_name="$1"
    path="$2"
    if [ -e "$2" ]; then
        export "$1=$2"
    fi
}

execute_if_command_exists() {
    local command=$1
    local executable_code=$2
    if command -v $command >/dev/null 2>&1; then
        eval "$executable_code"
    fi
}

source_file_if_exists "$HOME/.nix-profile/etc/profile.d/nix.sh"
source_file_if_exists "$HOME/.profile"
source_file_if_exists "$HOME/.bashrc"
source_file_if_exists "$HOME/.travis/travis.sh"
source_file_if_exists "/usr/local/etc/profile.d/autojump.sh"

# Slow:
# source_file_if_exists "/usr/local/bin/virtualenvwrapper.sh"
# source_file_if_exists "/usr/local/etc/bash_completion.d/password-store"
execute_if_command_exists "fasd" 'eval "$(fasd --init auto)"'

add_to_path_if_file_exists "PATH" "$HOME/Library/Haskell/bin"
add_to_path_if_file_exists "PATH" "$HOME/dotfiles/bin"
add_to_path_if_file_exists "LIBRARY_PATH" "/usr/local/lib:/Users/wmmc/.nix-profile/lib"
add_to_path_if_file_exists "CPATH" "/usr/local/include/:/Users/wmmc/.nix-profile/include"
add_to_path_if_file_exists "PATH" "$HOME/.bin"
add_to_path_if_file_exists "PATH" "$HOME/dotfiles_william_clarke/bin"
add_to_path_if_file_exists "PATH" "/Applications/Postgres.app/Contents/Versions/latest/bin"
add_to_path_if_file_exists "PATH" "$HOME/.cargo/bin"
add_to_path_if_file_exists "PATH" "/usr/local/sbin"
set_if_file_exists "EDITOR" "$HOME/.bin/edit"
set_if_file_exists "RUST_SRC_PATH" "$HOME/.multirust/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src/"
set_if_file_exists "ORDERWEB_HOME" "$HOME/deliveroo/orderweb"

export HEROKU_ORGANIZATION=deliveroo
