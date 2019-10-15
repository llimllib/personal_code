EDITOR=$(which vim)
export EDITOR

# If we're running tmux, change the TERM and open vim with reattach-to-user-namespace so that cnp works
[ -n "$TMUX" ] && export TERM=screen-256color

#get git completion script and branch prompt
[ -f /usr/local/etc/bash_completion.d/git-completion.bash ] && source /usr/local/etc/bash_completion.d/git-completion.bash
[ -f /usr/local/etc/bash_completion.d/git-prompt.sh ] && source /usr/local/etc/bash_completion.d/git-prompt.sh

# brew install bash-completion2
# gets you autocompletions in make, and in many many other programs:
# https://github.com/scop/bash-completion/tree/master/completions
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

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
alias glg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
alias prune='git remote prune origin'
alias pr="hub pull-request -o"
alias dc="docker-compose"
alias c="clear"
alias da="direnv allow"
alias de="direnv edit"
alias tf="terraform"
alias jless="jq -C '.' | less"
alias rg="rg --max-columns=250 --max-columns-preview --smart-case"

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
# removed because it's too slow
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

# prefer local/bin and local/sbin to bin
export PATH=/usr/local/bin:/usr/local/sbin:$PATH

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

# rbenv
if command -v rbenv 1>/dev/null 2>&1; then eval "$(rbenv init -)"; fi

# pyenv
if command -v pyenv 1>/dev/null 2>&1; then eval "$(pyenv init -)"; fi

# nodenv
if command -v nodenv 1>/dev/null 2>&1; then eval "$(nodenv init -)"; fi

GPG_TTY=$(tty)
export GPG_TTY

# --files: List files that would be searched but do not search
# --hidden: Search hidden files and folders
# --follow: Follow symlinks
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow'

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# run direnv https://direnv.net/
if command -v direnv 1>/dev/null 2>&1; then eval "$(direnv hook bash)"; fi
