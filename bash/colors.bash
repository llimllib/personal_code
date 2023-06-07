#!/usr/bin/env bash
# adapted from
# https://github.com/dylanaraps/neofetch/blob/master/neofetch#L577
#
# useful reference: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797

# TODO: mess with 24-bit codes:
# ESC[38;2;{r};{g};{b}m	Set foreground color as RGB.
# ESC[48;2;{r};{g};{b}m	Set background color as RGB.
#
# this function may be a good starting point for that:
# https://gist.github.com/mhulse/b11e568260fb8c3aa2a8

# reset all styles (incl. bold/etc)
reset='\e[0m'
bold='\e[1m'
faint='\e[2m'
italic='\e[3m'
underline='\e[4m'
blink='\e[5m'
inverse='\e[7m'
hidden='\e[8m'
strike='\e[9m'

# set a foreground color. Legal values for $1 are 0-255; terminals may
# interpret the value mod 255
color() {
    case $1 in
        [0-6])    printf '%b\e[3%sm'   "$reset" "$1" ;;
        7 | "fg") printf '\e[37m%b'    "$reset" ;;
        *)        printf '\e[38;5;%bm' "$1" ;;
    esac
}

# print every foreground color
printf "\n%b%bforeground colors%b\n\n" "$(color 1)" "$italic" "$reset"
rows=16
for ((i=0;i<rows;i++));
do
    for ((j=0;j<17;j++));
    do
        c="$(color "$((i*rows+j))")"
        printf "%b%03d " "$c" "$((i*rows+j))"
    done
    echo
done

# set a foreground and optionally a background color. Legal values for $1 or $2
# are 0-255; terminals may interpret the value mod 255
color2() {
    # 30-36 are the base colors
    # 37 is foreground
    # 38;5 lets you set a 256-color code
    case $1 in
        [0-6])    printf '%b\e[3%sm'   "$reset" "$1" ;;
        7 | "fg") printf '\e[37m%b'    "$reset" ;;
        *)        printf '\e[38;5;%bm' "$1" ;;
    esac
    if [ -n "$2" ]; then
        # 40-46 are the base colors
        # 47 is background
        # 48;5 lets you set a 256-color code
        case $2 in
            [0-7])    printf '\e[4%sm'   "$2" ;;
            *)        printf '\e[48;5;%bm' "$2" ;;
        esac
    fi
}

# TODO: use a dark text color on light bgs and vice-versa
#
# print every background color
printf "\n\n%b%bbackground colors%b\n\n" "$(color 1)" "$italic" "$reset"
rows=16
for ((i=0;i<rows;i++));
do
    for ((j=0;j<17;j++));
    do
        c="$(color2 fg "$((i*rows+j))")"
        printf "%b%03d " "$c" "$((i*rows+j))"
    done
    # echo
    printf "%b\n" "$reset"
done

# all switches have resets, but I'd say just reset all with $reset and add your
# style again. Bold's reset is '\e22m', for example
#
# see table here:
# https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#colors--graphics-mode
c=$(color 2)

printf "\n\n%b%btext styles%b\n\n" "$(color 1)" "$italic" "$reset"
printf "%bThis is %bbold text%b%b but this is not\n" "$c" "$bold" "$reset" "$c"
printf "%bThis is %bfaint text%b%b but this is not\n" "$c" "$faint" "$reset" "$c"
printf "%bThis is %bitalic text%b%b but this is not\n" "$c" "$italic" "$reset" "$c"
printf "%bThis is %bunderlined text%b%b but this is not\n" "$c" "$underline" "$reset" "$c"
printf "%bThis is %bblink text%b%b but this is not\n" "$c" "$blink" "$reset" "$c"
printf "%bThis is %binverse text%b%b but this is not\n" "$c" "$inverse" "$reset" "$c"
printf "%bThis is %bhidden text%b%b but this is not\n" "$c" "$hidden" "$reset" "$c"
printf "%bThis is %bstrikethrough text%b%b but this is not\n" "$c" "$strike" "$reset" "$c"

# Here's how asdf's nodejs manager does it:
# colored() {
#   local color="$1" text="$2"
#   printf "\033[%sm%s\033[39;49m\n" "$color" "$text"
# }
# 
# export RED=31 GREEN=32 YELLOW=33 BLUE=34 MAGENTA=35 CYAN=36
#
# printf "$(colored $CYAN "Installing the following default packages globally: ")"

