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
yellow="$(color 3)"
blue="$(color 4)" 
fg="$(color fg)"

# if the directory for the file $1 doesn't exist, create it
function ensuredir {
    if [ ! -d "$(dirname "$1")" ]; then
        mkdir -p "$1"
    fi
}

function warn {
    printf '%swarning:%s %s\n' "$yellow" "$fg" "$1" >&2
}

function info {
    printf '%s%s %s\n' "$blue" "$fg" "$1" >&2
}

# bitwarden items to write to disk, as "item name|destination path". SSH key
# items write the private key to the destination and the public key to
# $dest.pub; other items write their notes field.
private_items=(
    "agent commit signing key|${XDG_DATA_HOME:-$HOME/.local/share}/agent-ssh/id_ed25519"
)

# write $2 to the file $1, readable only by me, if it has changed
function write_private {
    if [ -f "$1" ] && [ "$(cat "$1")" == "$2" ]; then
        return
    fi
    # shellcheck disable=SC2174 # only the innermost dir should be private
    mkdir -p -m 700 "$(dirname "$1")"
    (umask 077 && printf '%s\n' "$2" > "$1")
    chmod 600 "$1"
    echo "wrote $1"
}

# pull private files out of bitwarden. Warns and skips rather than failing if
# bw isn't available or unlocked
function sync_private {
    info "syncing private items"

    if ! command -v bw &> /dev/null; then
        warn "bw not found, skipping private files"
        return
    fi
    if ! command -v jq &> /dev/null; then
        warn "jq not found, skipping private files"
        return
    fi

    case "$(bw status 2> /dev/null | jq -r .status)" in
        unlocked) ;;
        locked)
            warn "bitwarden is locked, skipping private files. Run: export BW_SESSION=\"\$(bw unlock --raw)\""
            return;;
        *)
            warn "not logged in to bitwarden (bw login), skipping private files"
            return;;
    esac

    bw sync &> /dev/null || warn "bw sync failed, using cached vault"

    for entry in "${private_items[@]}"; do
        local name="${entry%%|*}" dest="${entry#*|}" item
        if ! item=$(bw get item "$name" 2> /dev/null); then
            warn "couldn't get bitwarden item \"$name\", skipping"
            continue
        fi

        # type 5 is an SSH key item
        if [ "$(jq -r .type <<< "$item")" == 5 ]; then
            write_private "$dest" "$(jq -r .sshKey.privateKey <<< "$item")"
            write_private "$dest.pub" "$(jq -r .sshKey.publicKey <<< "$item")"
        else
            write_private "$dest" "$(jq -r .notes <<< "$item")"
        fi
    done
}

function main {
    git pull

    # find all files, except this file
    for f in $(fd --exclude "$(basename "$0")" --exclude syncignore --type file --hidden .); do
        src="$f"
        dest="$HOME/$f"

        # TODO: we might want to do something smarter than exact name matching,
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
                        # we need dirname here because otherwise we get a command like:
                        # rsync .config/git/exclude /Users/llimllib/.config/git/exclude
                        # which causes rsync to create a folder `exclude` in
                        # the git directory, then add the file `exclude` to it
                        rsync "$src" "$(dirname "$dest")";;
                esac
            fi
        fi
    done

    dirs_to_sync=(.local/bin .config/nvim/lua .config/mise)
    for dir in "${dirs_to_sync[@]}"; do
        for f in $(cd "$HOME" && fd --type file --hidden . "$dir"); do
            basename_="$(basename "$f")"
            # if the file exists and its basename isn't in syncignore
            if [ ! -f "$f" ] && ! grep -qE "^$basename_$" syncignore ; then
                read -r -p "Do you want to ${red}[a]${fg}dd $f to repo? " action
                case $action in
                    a)
                        ensuredir "./$f"
                        # we need dirname here for the same reason as above
                        rsync "$HOME/$f" "$(dirname "./$f")"
                        git add "./$f";;
                esac
            fi
        done
    done

    sync_private
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
