# add homebrew bin, and prefer local/bin and local/sbin to bin. git-prompt
# depends on being able to find brew, so this must come before that.
export PATH=/opt/homebrew/bin:/usr/local/bin:/usr/local/sbin:$PATH

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

function title {
  if [ -z "$TMUX" ] ; then
    printf "\\e]1;%s\\a" "$@"
  else
   tmux rename-window "$@"
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
alias jless="jq -C '.' | less"
alias dmc="docker-compose run --rm mctapi"
alias rg="rg --max-columns=250 --max-columns-preview --smart-case"
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
alias br='bundle exec rspec'
alias bi='bundle install'

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

# fd - use fzf to cd to selected directory
# https://github.com/junegunn/fzf/wiki/examples#changing-directory
fd() {
  local dir
  dir=$(find "${1:-.}" -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir" || exit
}

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

#lex
export LEX_DATA=/usr/local/share/lex/data
export LEX_DB=/usr/local/share/lex/db

#set golang root dir
export GOPATH=~/go
export GOBIN=~/go/bin

#
# PATH CONFIGURATION SECTION
#

# golang binaries
export PATH=$PATH:$GOPATH/bin

# haskell binaries
export PATH=$PATH:$HOME/.cabal/bin

# ruby binaries to path
export PATH=$PATH:/usr/local/opt/ruby/bin

# rust binaries
export PATH="$PATH:$HOME/.cargo/bin"

# yarn (javascript) binaries
export PATH="$PATH:$HOME/.yarn/bin"

# prefer n's version of node to /usr/local/bin/node
export PATH="$HOME/bin:$PATH"

#asdf
if [[ -f $HOME/.asdf/asdf.sh ]]; then
    . $HOME/.asdf/asdf.sh
    . $HOME/.asdf/completions/asdf.bash
fi

GPG_TTY=$(tty)
export GPG_TTY

# --files: List files that would be searched but do not search
# --hidden: Search hidden files and folders
# --follow: Follow symlinks
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow'

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# run direnv https://direnv.net/
if command -v direnv 1>/dev/null 2>&1; then eval "$(direnv hook bash)"; fi

# Create a worktree from a given branchname, in exactly the way I like it.
# oct 18, 2019
function worktree {
    set -x
    branchname="$1"

    # Replace slashes with underscores. If there's no slash, dirname will equal
    # branchname. So "alu/something-other" becomes "alu_omething-other", but
    # "quick-fix" stays unchanged
    # https://www.tldp.org/LDP/abs/html/parameter-substitution.html
    dirname=${branchname//\//_}

    # pull the most recent version of the remote
    git pull

    # if the branch name already exists, we want to check it out. Otherwise,
    # create a new branch. I'm sure there's probably a way to do that in one
    # command, but I'm done fiddling with git at this point
    if git branch -a | grep -E "\/$branchname" > /dev/null 2>&1; then
        git worktree add "../$dirname" "$branchname" || return
    else
        # otherwise, create a new branch
        git worktree add -b "$branchname" "../$dirname" || return
    fi

    cp .envrc "../$dirname"
    if [ -d node_modules ]; then
        ln -sf "$(pwd)/node_modules" "../$dirname/node_modules"
    fi
    if [ -f .env ]; then
        cp .env "../$dirname/"
    fi
    if [ -f .tool-versions ]; then
        cp .tool-versions "../$dirname/"
    fi
    cd "../$dirname" || return
    direnv allow
    set +x
}

# rmtree <dir> will remove a worktree's directory, then prune the worktree list
# and delete the branch
# Jul 8 2020
function rmtree {
    set -x
    if [ ! -d "master" ] && [ ! -d "main" ]; then
        echo "you must be in a directory with a master or main branch"
    fi
    if [ -z "$1" ] || [ ! -d "$1" ]; then
        echo "You must provide a directory name that is a worktree to remove"
        exit 1
    fi

    if [ -d "master" ]; then
        main_branch="master"
    else
        main_branch="main"
    fi

    branch_name=$(cd "$1" && git rev-parse --abbrev-ref HEAD)
    rm -rf "$1"
    (cd $main_branch && git worktree prune && git branch -D "$branch_name")
    set +x
}

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
    # separate from the assignment to make shellcheck happy
    jh=$(/usr/libexec/java_home 1>/dev/null 2>&1)
    # if there are no javas installed, java_home will exit with a nonzero
    # status code. in that case, do nothing
    if [[ $? -eq 0 ]]; then
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
