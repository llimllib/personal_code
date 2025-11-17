# shellcheck disable=SC1090
# shellcheck shell=zsh

# cd to a dir without need to type cd
setopt autocd

# This causes ^ to get interpreted as a special character, which is a pain in
# npm and git, so I've disabled it.
# https://zsh.sourceforge.io/Doc/Release/Options.html
# setopt extendedglob

# case insensitive matching
unsetopt case_match
# For some reason this causes spurious matches on my system. investigating it
# in a clean zsh, I was able to reproduce only after I imported and ran
# compinit:
#
# $ env -i HOME=$HOME PATH=$PATH USER=$USER TERM=$TERM TERMINFO=$TERMINFO zsh -d -f -i
# adama% autoload -Uz compinit && compinit
# adama% ls webpack*
# webpack.config.js	webpack.directive.js
# adama% ls webpack.config.js
# webpack.config.js
# adama% unsetopt case_glob
# adama% ls webpack.D
#
# where is that webpack.D completion coming from? It happens when I type
# webp<TAB> only after case_glob is unset.
#
# Then if you tab again, it will only autocomplete webpack.directive.js
#
# for now, I'm just going to disable this because I'm too lazy to file an issue
# against zsh, I probably (?) am misunderstanding compinit?
# unsetopt case_glob

# keep common commands out of the shell history
#
# This is from some rando zshrc's on github + the manual (man 1 zshparam). I do
# not know what I"m doing here. It is much simpler in bash
setopt extended_history
setopt hist_allow_clobber
setopt hist_fcntl_lock
setopt hist_find_no_dups 
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_no_functions
setopt hist_no_store

setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
setopt inc_append_history_time


# after all that, this still doesn't keep commands out of my terminal
export HISTORY_IGNORE="(ls|bg|fg)"
function zshaddhistory() {
	emulate -L zsh
	[[ ${1} != ${~HISTORY_IGNORE} ]]
}

# start compinit for completions. Only load the cache freshly once a day -
# otherwise you pay ~100ms on every zsh startup
# https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-2308206
autoload -Uz compinit
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

# set git up for prompt
# https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Zsh
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
# left version
zstyle ':vcs_info:git:*' formats '%K{yellow}%F{black}  %b %F{yellow}'
# right version
# zstyle ':vcs_info:git:*' formats '%F{yellow}%F{black}%K{yellow}  %b %F{yellow}'

# man zshoptions
#q
# MENU_COMPLETE (-Y)
#  On an ambiguous completion, instead of listing possibilities or beeping,
#  insert the first match immediately. 
#
# I tried this but found it too annoying, if I type pack<tab> I don't want the
# prompt to jump to package-lock.json
# setopt menu_complete

# ^x^e to edit the current command in $EDITOR... is there a less terrible
# shortcut? ^E doesn't work, that's end. I'll probably never remember this
# shortcut but it does work pretty well
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# fancy lscolors (needs to come before the menu option I think)
export LSCOLORS=dxfxcxdxbxegedabagacad

# better completion menu
# https://thevaluable.dev/zsh-completion-guide-examples/
zstyle ':completion:*' menu select    
zstyle ':completion:*:default' list-colors ${(s.:.)LSCOLORS}

# case insensitive completion
# https://superuser.com/a/1092328/55099
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

# fancy hand-made powerline-style prompt. The trick is to switch the foreground
# color of the arrow to the background color of the preceding section, and the
# background color of the arrow to the background of the following section
#
# Here's a printf that may help demonstrate the effect:
# printf "%b%bhome\xee\x82\xb1some-branch%b\n" "\e[30m" "\e[44m" "\e[0m"
# however, we can't use those same sorts of escapes in a PROMPT, so we use
# zsh's instead
#
# The %(5~|...) thing shortens the path when you're deep in a tree to:
# `first segment/.../last/three/segments`, otherwise displays four segments
# https://unix.stackexchange.com/a/273567
#
# %(x.true-text.false-text) is the ternary operator in zsh, we use it for
# handing the return code and number of jobs in the shell
#
# https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html
# `man zshmisc`
#
function makeprompt() {
    local prompt=''

    # status of last command. Green check if success, Red code if failure
    prompt+="%(?.%K{green}%F{black}√%F{green}.%K{red}%F{black}%?%F{red})"

    # if there are more than one backgorund job, add them to the line
    prompt+="%(1j.%K{magenta}%F{white}𑫕%j%F{magenta}.)"

    # machine name in blue
    prompt+="%K{blue}%F{black}  %m %F{blue}"

    # current directory, limited to 4 segments, in green
    prompt+="%K{green}%F{black}   %(5~|%-1~/…/%3~|%4~) %F{green}"

    prompt+='${vcs_info_msg_0_}'
 
    # reset colors
    LF=$'\n'
    prompt+="%k%f ${LF}$ "
 
    echo "$PROMPT"
}
 
# to debug:
# echo "$(makeprompt)"
export PS1="$(makeprompt)"

# old, not fancy prompt:
# export PROMPT='%(?.%F{green}√.%F{red}?%?)%f %F{blue}%m:%F{green}%~%f $ '

# The right prompt should be on the same line as the first line of the left
# prompt. To do so, there is just a quite ugly workaround: Before zsh draws
# the RPROMPT, we advise it, to go one line up. At the end of RPROMPT, we
# advise it to go one line down. See:
# http://superuser.com/questions/357107/zsh-right-justify-in-ps1
local RPROMPT_PREFIX='%{'$'\e[1A''%}' # one line up
local RPROMPT_SUFFIX='%{'$'\e[1B''%}' # one line down

# show the time on the right
# export RPROMPT='%F{blue}%t'
# with git branch (currently moved to left)
# export RPROMPT='${vcs_info_msg_0_}%F{magenta}%F{black}%K{magenta} %t'
export RPROMPT="${RPROMPT_PREFIX}%F{magenta}%F{black}%K{magenta}%t${RPROMPT_SUFFIX}"

# remove the useless space at the right of rprompt
# https://superuser.com/a/726509/55099
export ZLE_RPROMPT_INDENT=0

# on c-l, if we're using kitty, send the magical 22J escape that clears
# scrollback but doesn't destroy it. Not sure why that's not the default
# :shrug:
#
# https://github.com/kovidgoyal/kitty/blob/cf0d3080/kitty/options/definition.py#L4015
if [ "$TERM" = "xterm-kitty" ]; then
    # it is very frustrating that this does not already work properly in kitty
    # and I cannot get it working exactly like I want.
    #
    # Discussion with the author:
    # https://github.com/kovidgoyal/kitty/discussions/6460
    clear-screen-saving-contents-in-scrollback() {
        # this works if you run it at the command prompt, but inside a widget
        # it leaves you without a prompt:
        printf "\e[H\e[22J"

        # these work to get your prompt back, but add an extra line to the
        # scrollback:
        zle .reset-prompt
        # zle redisplay
        # zle clear-screen
        #
        # this works, but leaves a blank line at the top:
        # zle accept-line
        #
        # these don't work:
        # zle kill-line
        # zle end-of-line
        # zle get-line
        #
        # this puts the printf call onto the prompt, which adds it to the
        # history, which I don't want:
        # zle -U $'printf "\\e[H\\e[22J"'
        # zle accept-line
    }

    zle -N clear-screen-saving-contents-in-scrollback
    bindkey '^l' clear-screen-saving-contents-in-scrollback
fi

# it's possible to get a good two-line prompt with a right side, but a bunch of
# work. Example:
# https://gist.github.com/romkatv/2a107ef9314f0d5f76563725b42f7cab

# in vim terminal, ctrl-a doesn't work unless I set this. I'm a vim user who
# uses emacs bindings at the prompt :shrug:
bindkey -e

function mkcd {
    mkdir "$1" && cd "$1" || exit
}

function title {
  if [ -n "$KITTY_PID" ]; then
    kitty @ set-tab-title "$@"
  elif [ -n "$TMUX" ] ; then
   tmux rename-window "$@"
  else
   printf "\\e]1;%s\\a" "$@"
  fi
}

YELLOW='\e[33m'
RESET='\e[0m'

# open a parquet file with duckdb
function parduck {
    local filename="$1"
    local tablename="${filename%.parquet}"
    printf "creating table %b%s%b\n" $YELLOW "$tablename" $RESET
    duckdb -init <(echo "CREATE TABLE $tablename AS SELECT * FROM '$filename';")
}

# set the default XDG directories; this shouldn't be necessary but some apps
# use the presence of these variables to decide whether to follow XDG or not
export XDG_DATA_HOME=~/.local/share
export XDG_CONFIG_HOME=~/.config
export XDG_STATE_HOME=~/.local/state
export XDG_CACHE_HOME=~/.cache

# PATH CONFIGURATION
#
# PATH configs have to go before aliases, some of them depend on the updated
# paths being available
export GOPATH=~/go
export GOBIN="$GOPATH/bin"

export LOCALBIN="$HOME/.local/bin"
export DOTNETBIN="$HOME/.dotnet/tools"
export HOMEBREWBIN="/opt/homebrew/bin"
export HOMEBREWSBIN="/opt/homebrew/sbin"
export PNPMBIN="$HOEM/Library/pnpm"

# add homebrew bin, go bin, and prefer local/bin and local/sbin to bin.
# git-prompt depends on being able to find brew, so this must come before that.
export PATH=$LOCALBIN:$HOMEBREWBIN:$HOMEBREWSBIN:$GOBIN:$PNPMBIN:/usr/local/bin:/usr/local/sbin:$PATH

# Fix colors in ipython paging
export PAGER="less"
export LESS="-SRXF"

# git aliases
alias gs='ls && echo "---------------------------------------" && git status'
alias gd="git diff"
alias gdc="git diff --cached"
alias gm="git co master"
alias gb="git branch"
alias gc="git checkout"
alias gm="git ci -m"
alias gma="git ci -a -m"
alias ga="git add"
alias gl="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit | head"
alias glg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
alias pr="gh pr create"
alias gist="gh gist create"
alias prune='git remote prune origin'

# if bat is present, replace cat with it
if command -v bat > /dev/null; then
    alias bat='bat --wrap never' # Add the `--wrap never` arg to all `bat` invocations
    alias cat='bat --wrap never'

    # tail a file with syntax highlighting and line numbers. Not a tail alias
    # because it can't do -f
    bail() {
        bat --color=always --paging=never "$1" | tail "${@:2}"
    }

    # use bat to read man pages
    # https://github.com/sharkdp/bat#man
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"

    # create a function that pipes llm output through bat to highlight markdown
    llm() {
      command llm "$@" | bat --paging=never --style=plain --language=markdown
    }
fi

alias be='bundle exec'
# https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md
alias gg='batgrep'
# use rsync for copying: experimental (and verbose, but progress!)
# skip for now on linux because it complains about crtimes
[[ $(uname) == "Darwin" ]] && alias cp='command rsync --human-readable --progress --archive --hard-links --acls --crtimes --rsh=/dev/null --one-file-system --backup --backup-dir=/tmp/rsync --'
alias dc='docker compose'
alias c='clear'
alias clean='env -i HOME=$HOME PATH=$PATH USER=$USER'
alias erd="erd -y inverted --human " # give erd a better default sort
alias icat='kitty +kitten icat'
if command -v eza > /dev/null; then
    export EZA_ICON_SPACING=2
    alias ls='eza --icons=auto --hyperlink'
# if gls (gnu ls installed by homebrew) is present, prefer it to ls. If it's
# not, use --hyperlink if it's available; otherwise back to bsd ls options
elif command -v gls >/dev/null ; then
    alias ls='gls -FG --hyperlink=auto --color=auto'
else
    if ls --help | grep hyperlink; then
        alias ls='ls -FG --hyperlink=auto --color=auto'
    else
        alias ls='ls -FG'
    fi
fi
alias py='ipython'
alias rg="rg --max-columns=250 --max-columns-preview --smart-case --hidden --glob '!.git' --hyperlink-format=kitty"
alias sqlite='sqlite3'
alias tf='terraform'
alias tmux='tmux -2' # tmux into 256 color mode
alias ts='npx ts-node'
alias vim='nvim'
alias lvim='NVIM_APPNAME=LazyVim_starter nvim'
alias run='npm run'

# cd into a readme project
alias rd='. ~/.local/bin/,rd'

# ask claude a question and get an answer without ceremony
alias q='llm --system "respond with a simple answer and do not explain yourself at all. Do not quote the answer" '

# "get-headers https://billmill.org" to do a GET request and print the headers
# and total time of the response
# https://github.com/carlmjohnson/get-headers/issues/4
alias get-headers="curl -sD - -o /dev/null -w 'time:\t%{time_total}s\ndownload speed:\t%{speed_download} bytes/s'"

# this BAT_THEME makes it use the colors you've already got defined in your
# terminal
export BAT_THEME="ansi"

# has to happen after the path gets modified, or else nvim may not be found
EDITOR=$(which nvim)
export EDITOR

# on mac, I'm getting SHELL=bash inside my zsh sessions and the hell if I can
# understand why. Anyway, set the default to zsh.
export SHELL=zsh

# run mise activate if it's available
#
# https://mise.jdx.dev/getting-started.html
# https://mise.jdx.dev/dev-tools/comparison-to-asdf.html
mise=$(command -v mise)
[[ -n $mise ]] && eval "$($mise activate zsh)"

# fzf
#
# use the presence of fd to signal the ability to set complex fzf settings.
# Full prereqs: fd erd bat
if command -v fd &> /dev/null ; then
    # https://github.com/junegunn/fzf?tab=readme-ov-file#environment-variables
    export FZF_DEFAULT_COMMAND='fd --type f'
    export FZF_DEFAULT_OPTS="--ansi \
          --height 100% \
          --layout reverse \
          --border bold \
          --preview 'fzf-preview {}' \
          --preview-window 'right,60%,border,+3/3'"

    # https://github.com/junegunn/fzf#settings
    #
    # Use fd (https://github.com/sharkdp/fd) instead of the default find
    # command for listing path candidates.
    # - The first argument to the function ($1) is the base path to start traversal
    # - See the source code (completion.{bash,zsh}) for the details.
    _fzf_compgen_path() {
      fd --hidden --follow --exclude ".git" . "$1"
    }

    # Use fd to generate the list for directory completion
    _fzf_compgen_dir() {
      fd --type d --hidden --follow --exclude ".git" . "$1"
    }

    # This tells fzf what to do when the uesr presses **
    _fzf_comprun() {
      local command=$1
      shift

      fzf --ansi \
          --height 40% \
          --layout reverse \
          --border bold \
          --preview 'fzf-preview {}' \
          --preview-window 'right,60%,border,+3/3' \
          "$@"
    }
fi

# install fzf with `/opt/homebrew/opt/fzf/install --xdg` to put it in the xdg
# config dir instead of the home root
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.zsh

# direnv https://direnv.net/
if command -v direnv 1>/dev/null 2>&1; then eval "$(direnv hook zsh)"; fi

# mac sets an absurdly low file handle limit of 256, and apparently you
# need to set it to this value? I haven't tested, but whateves this
# should get the job done.
# https://discussions.apple.com/thread/251000125
ulimit -n 10240

# atuin
# https://github.com/ellie/atuin
#
# https://atuin.sh/docs/config/key-binding
# says that by default, atuin will bind both ctrl-r and the up arrow, but this
# wasn't happening on zsh, so let's manually set ctrl-r
export ATUIN_NOBIND="true"
eval "$(atuin init zsh)"
bindkey '^r' _atuin_search_widget

[[ -f ~/.cargo/env ]] && source ~/.cargo/env

# You might want to set export PIPENV_VENV_IN_PROJECT=1 in your .bashrc/.zshrc
# (or any shell configuration file) for creating the virtualenv inside your
# project’s directory, avoiding problems with subsequent path changes.
export PIPENV_VENV_IN_PROJECT=1

# pnpm
if [[ -d "$HOME/Library/pnpm" ]]; then
    export PNPM_HOME="/Users/llimllib/Library/pnpm"
    export PATH="$PNPM_HOME:$PATH"
fi

# delta (https://github.com/dandavison/delta/issues/359)
export DELTA_FEATURES
function delta_sidebyside {
    if [[ "${COLUMNS}" -ge 120 ]]; then
        DELTA_FEATURES="side-by-side"
    else
        DELTA_FEATURES=""
    fi
}
trap delta_sidebyside WINCH
delta_sidebyside

# bun completions
[ -s "/Users/llimllib/.bun/_bun" ] && source "/Users/llimllib/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# pnpm
export PNPM_HOME="/Users/llimllib/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# television looks neat, but doesn't support bat's ansi mode currently:
# https://github.com/alexpasmantier/television/issues/271
# if the television config file is available, source it
# [[ -f ~/.config/television/init.zsh ]] && source ~/.config/television/init.zsh
