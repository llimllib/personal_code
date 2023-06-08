# shellcheck disable=SC1090
# shellcheck shell=bash

# add homebrew bin, and prefer local/bin and local/sbin to bin. git-prompt
# depends on being able to find brew, so this must come before that.
export PATH=$HOME/.local/bin:/opt/homebrew/bin:/usr/local/bin:/opt/homebrew/sbin:/usr/local/sbin:$PATH

EDITOR=$(which nvim)
export EDITOR

# If we're running tmux, change the TERM and open vim with reattach-to-user-namespace so that cnp works
[ -n "$TMUX" ] && export TERM=screen-256color

#get git completion script and branch prompt
[ -f /opt/homebrew/etc/bash_completion.d/git-completion.bash ] && source /opt/homebrew/etc/bash_completion.d/git-completion.bash
[ -f /opt/homebrew/etc/bash_completion.d/git-prompt.sh ] && source /opt/homebrew/etc/bash_completion.d/git-prompt.sh

# brew install bash-completion@2
# gets you autocompletions in make, and in many many other programs:
# https://github.com/scop/bash-completion/tree/master/completions
[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

[ -f ~/.bash.local.sh ] && source ~/.bash.local.sh

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
alias ls='ls -FG'
export LSCOLORS=dxfxcxdxbxegedabagacad
export LSCOLORS

# Fix colors in ipython paging
export PAGER="less"
export LESS="-SRXF"

alias sqlite='sqlite3'
PS1='\[\e[36m\]\@\[\e[0m\] \[\e[32m\]\H:\w\[\e[0m\] \[\e[0;31m\]$(__git_ps1 " %s")\[\e[0m\] \n\$ '

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
alias gpu='git push -u origin $(git rev-parse --abbrev-ref HEAD)'
alias gph="git push heroku"
alias gl="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit | head"
alias glg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
alias prune='git remote prune origin'
alias pr="gh pr create"
alias draft="hub pull-request -d -o"
alias dc="docker compose"
alias c="clear"
alias da="direnv allow"
alias de="direnv edit"
alias tf="terraform"
alias ts="npx ts-node"
alias dmc="docker-compose run --rm mctapi"
alias rg="rg --max-columns=250 --max-columns-preview --smart-case --hidden --glob '!.git'"
alias govim="vim -u ~/.govimrc"
alias vim="nvim"

alias clean='env -i HOME=$HOME PATH=$PATH USER=$USER'

# do `author <branch_name>` to get the most recent committer on a branch
alias author="git --no-pager log -1 --pretty=format:'%aN <%aE>'"

# do `squash <branch_name>` to squash and stage the branch, then put you in
# an editor to amend the commit message
function squash {
    git merge --squash "$1" && git commit --author "$(author "$1")"
}

alias be='bundle exec'

alias hs='ls && echo "---------------------------------------" && hg status'
alias hd='hg diff'
alias hm='hg ci -m'

# blog.burntsushi.net/ripgrep/
alias rack="rg --type ruby"
alias jack="rg --type js --type html"
alias pack="rg --type py"
alias cack="rg --type c --type cpp"
alias gack="rg --type go"
alias aack="rg -i"

alias t="tree | less"

alias s="sl"
alias l="sl"

alias ls='ls -FG'

alias py="ipython"

alias hubvan="ssh llimllib@159.203.101.116"

alias be="bundle exec"

# makes tmux colors closer to correct. But still bizarre.
alias tmux="tmux -2"

# Add the `--wrap never` arg to all `bat` invocations
alias bat="bat --wrap never"
export BAT_THEME="base16"

# give erd a better default sort
alias erd="erd --inverted"

# find, case insensivitely, in the current dir
function f {
    find . -iname "*$1*"
}

export CVS_RSH=ssh

export MANPATH=$MANPATH:/opt/local/man

# case insensitive matching
shopt -s nocaseglob
# ** matching
shopt -s globstar
# cd to a dir without need to type cd
shopt -s autocd
# extended globbing
shopt -s extglob
# append to history instead of overwriting
shopt -s histappend
# Save multi-line commands as one command
shopt -s cmdhist

# Before each bash prompt, write to history and read from it. This
# makes multiple terminals sync to history
# PROMPT_COMMAND='history -a; history -n'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

#set golang root dir
export GOPATH=~/go
export GOBIN=~/go/bin

#
# PATH CONFIGURATION SECTION
#

# golang binaries
export PATH=$PATH:$GOPATH/bin

# yarn (javascript) binaries
export PATH="$PATH:$HOME/.yarn/bin"

#asdf
if [[ -d $HOME/.local/share/asdf ]]; then
    # I really wish asdf supported XDG_CONFIG:
    # https://github.com/asdf-vm/asdf/issues/687
    #
    # so let's set a bunch of variables that let us pretend it does
    export ASDF_DIR="$HOME/.local/share/asdf"
    export ASDF_DATA_DIR="$HOME/.local/share/asdf"

    . "$ASDF_DIR/asdf.sh"
    . "$ASDF_DIR/completions/asdf.bash"

    # https://asdf-vm.com/manage/configuration.html#asdfrc
    export ASDF_CONFIG_FILE="$HOME/.config/asdf/asdfrc"

    # https://github.com/asdf-vm/asdf-nodejs#default-npm-packages
    export ASDF_NPM_DEFAULT_PACKAGES_FILE="$HOME/.config/asdf/default-npm-packages"

    # https://github.com/asdf-community/asdf-python#default-python-packages
    export ASDF_PYTHON_DEFAULT_PACKAGES_FILE="$HOME/.config/asdf/default-python-packages"
fi

GPG_TTY=$(tty)
export GPG_TTY

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
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash

# run direnv https://direnv.net/
if command -v direnv 1>/dev/null 2>&1; then eval "$(direnv hook bash)"; fi

# Why /usr/libexec/java_home?
# This java_home can return the Java version specified in Java Preferences for the current user. For examples,
# 
# /usr/libexec/java_home -V
# Matching Java Virtual Machines (3):
#     1.7.0_05, x86_64:	"Java SE 7"	/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home
#     1.6.0_41-b02-445, x86_64:	"Java SE 6"	/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
#     1.6.0_41-b02-445, i386:	"Java SE 6"	/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
# 
# This Mac OSX has three JDK installed.
# 
# ##return top Java version
# $ /usr/libexec/java_home
# /Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home
# 
# ## I want Java version 1.6
# $ /usr/libexec/java_home -v 1.6
# /System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
if [[ -f /usr/libexec/java_home ]]; then
    # if there are no javas installed, java_home will exit with a nonzero
    # status code. in that case, do nothing
    if jh=$(/usr/libexec/java_home 1>/dev/null 2>&1); then
        export JAVA_HOME=$jh
        export PATH="$PATH:$JAVA_HOME/bin"
    fi
fi

# brew install google-cloud-sdk
# we'll use the presence of this file to decide whether this is applicable or
# not
if [[ -f "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc" ]]; then
    source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc"
    source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc"
fi

# mac sets an absurdly low file handle limit of 256, and apparently you
# need to set it to this value? I haven't tested, but whateves this
# should get the job done.
# https://discussions.apple.com/thread/251000125
ulimit -n 10240

##### set up atuin
# https://github.com/ellie/atuin
[[ -f ~/.bash-preexec.sh ]] && source ~/.bash-preexec.sh
# bind ctrl-r to atuin but not the up arrow. The next version will let you pass
# --disable-up-arrow to handle this automatically, but for now we have to start
# by disabling all bindings, then binding ctrl-r explicitly
#
# https://github.com/ellie/atuin/blob/main/docs/key-binding.md
export ATUIN_NOBIND="true"
eval "$(atuin init bash)"
bind -x '"\C-r": __atuin_history'
#
##### end atuin

[[ -f ~/.cargo/env ]] && source ~/.cargo/env

# You might want to set export PIPENV_VENV_IN_PROJECT=1 in your .bashrc/.zshrc
# (or any shell configuration file) for creating the virtualenv inside your
# project’s directory, avoiding problems with subsequent path changes.
export PIPENV_VENV_IN_PROJECT=1

# pnpm
export PNPM_HOME="/Users/llimllib/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end
