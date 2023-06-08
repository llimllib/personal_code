#!/usr/bin/env bash

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

# TODO: add some sort of ignore file for files I don't want to sync on this
# machine for w/e reason
#
# find all files, except this file
for f in $(fd --exclude "$(basename "$0")" --type file --hidden .); do
    src="$f"
    dest="$HOME/$f"
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
                    rsync "$dest" "$src";;
                r)
                    rsync "$src" "$dest";;
            esac
        # if the destination file doesn't exist, ask if they want to add it
        else
            read -r -p "Do you want to ${red}[a]${fg}dd $src to $dest? " action
            case $action in
                a)
                    rsync "$src" "$dest";;
            esac
        fi
    fi
done
