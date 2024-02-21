#!/usr/bin/env bash

function usage {
    cat <<EOF
sync.bash [-v]

sync my home directory to git in the way I prefer
EOF
    kill -INT $$
}

# adapted from
# https://github.com/dylanaraps/neofetch/blob/master/neofetch#L577
reset='\e[0m'
color() {
    case $1 in
        [0-6])    printf '%b\e[3%sm'   "$reset" "$1" ;;
        7 | "fg") printf '\e[37m%b'    "$reset" ;;
        *)        printf '\e[38;5;%bm' "$1" ;;
    esac
}

red="$(color 1)" 
green="$(color 2)" 
blue="$(color 4)" 
fg="$(color fg)"

# if the directory for the file $1 doesn't exist, create it
function ensuredir {
    if [ ! -d "$(dirname "$1")" ]; then
        mkdir -p "$1"
    fi
}

function main {
    git pull

    # find all files, except this file
    for f in $(fd --exclude "$(basename "$0")" --exclude syncignore --type file --hidden .); do
        src="$f"
        dest="$HOME/$f"

        # TOOD: we might want to do something smarter than exact name matching,
        # for now its good enough though
        basename_=$(basename "$f")
        if grep -qE "^$basename_$" syncignore ; then
            continue;
        fi

        # --exit-code: Make the program exit with codes similar to diff(1). That
        #              is, it exits with 1 if there were differences and 0 means no
        #              differences.
        if ! git diff --exit-code --quiet "$src" "$dest" &> /dev/null; then
            # if the destination file exists, show a diff and ask the user if they
            # want to accept the change from either side, or ignore it
            if [ -e "$dest" ]; then
                git diff "$dest" "$src" 
                read -r -p "Take ${red}[h]${fg}omedir version, ${green}[r]${fg}emote version, or ${blue}[i]${fg}gnore: " action
                case $action in
                    h)
                        ensuredir "$dest"
                        rsync "$dest" "$src";;
                    r)
                        ensuredir "$dest"
                        rsync "$src" "$dest";;
                esac
            # if the destination file doesn't exist, ask if they want to add it
            else
                read -r -p "Do you want to ${red}[a]${fg}dd $src to $dest? " action
                case $action in
                    a)
                        ensuredir "$dest"
                        rsync "$src" "$dest";;
                esac
            fi
        fi
    done

    dirs_to_sync=(.local/bin .config/nvim/lua)
    for dir in "${dirs_to_sync[@]}"; do
        for f in $(cd "$HOME" && fd --type file --hidden . "$dir"); do
            basename_="$(basename "$f")"
            # if the file exists and its basename isn't in syncignore
            if [ ! -f "$f" ] && ! grep -qE "^$basename_$" syncignore ; then
                read -r -p "Do you want to ${red}[a]${fg}dd $f to repo? " action
                case $action in
                    a)
                        ensuredir "$dir"
                        rsync "$HOME/$f" "./$f"
                        git add "./$f";;
                esac
            fi
        done
    done
}

while true; do
    case $1 in
        help | -h | --help)
            usage
            ;;
        -v | --verbose)
            set -x
            shift
            ;;
        *)
            break
            ;;
    esac
done

main "$@"
