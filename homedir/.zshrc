# shellcheck disable=SC1090
# shellcheck shell=zsh

# case insensitive matching
unsetopt case_glob
unsetopt case_match

# cd to a dir without need to type cd
setopt autocd

setopt extendedglob

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

# start compinit for completions
autoload -Uz compinit && compinit

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

# fancy lscolors (needs to come before the menu option I think)
export LSCOLORS=dxfxcxdxbxegedabagacad

# better completion menu
# https://thevaluable.dev/zsh-completion-guide-examples/
zstyle ':completion:*' menu select    
zstyle ':completion:*:default' list-colors ${(s.:.)LSCOLORS}

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
# https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html
#
 function makeprompt() {
     local prompt=''
     # status of last command. Green check if success, Red code if failure
     prompt+="%(?.%K{green}%F{black}√ %F{green}.%K{red}%F{black}%? %F{red})"

     # machine name in blue
     prompt+="%K{blue}%F{black}  %m %F{blue}"

     # current directory, limited to 4 segments, in green
     prompt+="%K{green}%F{black}  %(5~|%-1~/…/%3~|%4~) %F{green}"

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

# show the time on the right
# export RPROMPT='%F{blue}%t'
# with git branch (currently moved to left)
# export RPROMPT='${vcs_info_msg_0_}%F{magenta}%F{black}%K{magenta} %t'
export RPROMPT='%F{magenta}%F{black}%K{magenta}%t'

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

# Fix colors in ipython paging
export PAGER="less"
export LESS="-SRXF"

# TODO: git
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
alias gp="git push"
alias gl="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit | head"
alias glg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
alias pr="gh pr create"
alias prune='git remote prune origin'

# other aliases
alias bat='bat --wrap never' # Add the `--wrap never` arg to all `bat` invocations
alias be='bundle exec'
# https://github.com/eth-p/bat-extras/blob/master/doc/batgrep.md
alias gg='batgrep'
# use rsync for copying: experimental (and verbose, but progress!)
alias cp='command rsync --human-readable --progress --archive --hard-links --acls --crtimes --rsh=/dev/null --one-file-system --backup --backup-dir=/tmp/rsync --'
alias dc='docker compose'
alias c='clear'
alias clean='env -i HOME=$HOME PATH=$PATH USER=$USER'
alias da='direnv allow'
alias de='direnv edit'
alias erd="erd --inverted" # give erd a better default sort
alias icat='kitty +kitten icat'
alias ls='ls -FG'
alias py='ipython'
alias rg="rg --max-columns=250 --max-columns-preview --smart-case --hidden --glob '!.git'"
alias sqlite='sqlite3'
alias tf='terraform'
alias tmux='tmux -2' # tmux into 256 color mode
alias ts='npx ts-node'
alias vim='nvim'

# this BAT_THEME makes it use the colors you've already got defined in your
# terminal
export BAT_THEME="ansi"

#set golang root dir
export GOPATH=~/go
export GOBIN="$GOPATH/bin"

# PATH CONFIGURATION
#
# add homebrew bin, go bin, and prefer local/bin and local/sbin to bin.
# git-prompt depends on being able to find brew, so this must come before that.
export PATH=$HOME/.local/bin:/opt/homebrew/bin:/usr/local/bin:/opt/homebrew/sbin:/usr/local/sbin:$GOBIN:$PATH

# has to happen after the path gets modified, or else nvim may not be found
EDITOR=$(which nvim)
export EDITOR

#asdf
if [[ -d $HOME/.local/share/asdf ]]; then
    # I really wish asdf supported XDG_CONFIG:
    # https://github.com/asdf-vm/asdf/issues/687
    #
    # so let's set a bunch of variables that let us pretend it does
    export ASDF_DIR="$HOME/.local/share/asdf"
    export ASDF_DATA_DIR="$HOME/.local/share/asdf"

    . "$ASDF_DIR/asdf.sh"
    # https://asdf-vm.com/guide/getting-started.html#_3-install-asdf
    #
    # the instructions change if I were to start using oh-my-zsh
    fpath=(${ASDF_DIR}/completions $fpath)
    autoload -Uz compinit && compinit

    # https://asdf-vm.com/manage/configuration.html#asdfrc
    export ASDF_CONFIG_FILE="$HOME/.config/asdf/asdfrc"

    # https://github.com/asdf-vm/asdf-nodejs#default-npm-packages
    export ASDF_NPM_DEFAULT_PACKAGES_FILE="$HOME/.config/asdf/default-npm-packages"

    # https://github.com/asdf-community/asdf-python#default-python-packages
    export ASDF_PYTHON_DEFAULT_PACKAGES_FILE="$HOME/.config/asdf/default-python-packages"
fi

# fzf
#
# use the presence of fd to signal the ability to set complex fzf settings.
# Full prereqs: fd erd bat
if command -v fd &> /dev/null ; then
    export FZF_DEFAULT_COMMAND='fd --type f'

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

    # Advanced customization of fzf options via _fzf_comprun function
    # - The first argument to the function is the name of the command.
    # - You should make sure to pass the rest of the arguments to fzf.
    _fzf_comprun() {
      local command=$1
      shift

      # would love to use "imgcat" instead of "identify" for images, but fzf
      # doesn't support it unfortunately:
      # https://github.com/junegunn/fzf/issues/1102
      # shellcheck disable=SC2016
      case "$command" in
        cd)           fzf --preview 'erd --inverted -H -C {} | head -200'   "$@" ;;
        export|unset) fzf --preview "eval 'echo \$'{}"         "$@" ;;
        ssh)          fzf --preview 'dig {}'                   "$@" ;;
        *)            fzf --preview 'ft=$(file --mime-type -b {}); if [[ $ft == image* ]]; then identify {}; elif [[ $ft = inode* ]]; then erd -H -C {} | head -n100; else bat -n --color=always {}; fi;'
                                   # bat -n --color=always {} 2>/dev/null || imgcat {} 2>/dev/null || erd -H -C {} | head -200' "$@" ;;
      esac
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
export PNPM_HOME="/Users/llimllib/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
