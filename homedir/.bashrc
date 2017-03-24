export EDITOR=/usr/local/bin/vim
#export TERM='screen-256color'

# If we're running tmux, change the TERM and open vim with reattach-to-user-namespace so that cnp works
[ -n "$TMUX" ] && export TERM=screen-256color
function vim {
  if [ -n "$TMUX" ]
  then
     reattach-to-user-namespace vim $@
  else
     /usr/local/bin/vim $@
  fi
}

#get git completion script and branch prompt
source /usr/local/etc/bash_completion.d/git-completion.bash
source /usr/local/etc/bash_completion.d/git-prompt.sh

#export PROMPT_COMMAND='echo -ne "\033]0;\007"'
function title {
    echo -ne "\033]0;$1\007"
}

# I generally put little tmux scripts in my folders with the name ".start.sh";
# I don't want to run them automatically, so this command just sources them
# to set up my env
function mux {
    . .start.sh
}

alias ls='ls -FG'
export LSCOLORS=dxfxcxdxbxegedabagacad
export LSCOLORS

# Fix colors in ipython paging
export PAGER="less"
export LESS="-r -S"

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
alias gph="git push heroku"
alias glg="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
alias prune='git remote prune origin'
alias pr="hub pull-request -o"

alias clean='env -i HOME=$HOME PATH=$PATH USER=$USER'

# do `author <branch_name>` to get the most recent committer on a branch
alias author="git --no-pager log -1 --pretty=format:'%aN <%aE>'"

# do `squash <branch_name>` to squash and stage the branch, then put you in
# an editor to amend the commit message
function squash {
    git merge --squash $1 && git commit --author "`author $1`"
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

alias hubvan="ssh hubvan.com"

alias be="bundle exec"

# makes tmux colors closer to correct. But still bizarre.
alias tmux="tmux -2"

# find, case insensivitely, in the current dir
function f {
    find . -iname "*$1*"
}

export CVS_RSH=ssh

export MANPATH=$MANPATH:/opt/local/man

# http://www.mkyong.com/java/how-to-set-java_home-environment-variable-on-mac-os-x/
# Get this value with $(/usr/libexec/java_home)
# that command is too slow to run on every bash invocation, so set it here
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_20.jdk/Contents/Home

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

# several ideas from: https://github.com/mrzool/bash-sensible/blob/master/sensible.bash

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

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

export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

# run autoenv
if [[ -s /usr/local/bin/activate.sh ]] ; then source /usr/local/bin/activate.sh ; fi

#don't write pyc or pyo files
export PYTHONDONTWRITEBYTECODE=1

#lex
export LEX_DATA=/usr/local/share/lex/data
export LEX_DB=/usr/local/share/lex/db

#set golang root dir
export GOPATH=~/go

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

# perl binaries
export PATH="$PATH:$HOME/perl4/bin"

# yarn (javascript) binaries
export PATH="$PATH:$HOME/.yarn/bin"

# rbenv
eval "$(rbenv init -)"

# pyenv
eval "$(pyenv init -)"

# Start gpg-agent
#
# from https://github.com/ErinCall/Dotfiles/blob/master/.bashrc#L32-L40
# via
# https://blog.erincall.com/p/signing-your-git-commits-with-gpg
#
# kill -0 checks to see if the pid exists
if test -f $HOME/.gpg-agent-info && kill -0 `cut -d: -f 2 $HOME/.gpg-agent-info` 2>/dev/null; then
    GPG_AGENT_INFO=`cat $HOME/.gpg-agent-info | cut -c 16-`
else
# No, gpg-agent not available; start gpg-agent
    eval `gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info`
fi
export GPG_TTY=`tty`
export GPG_AGENT_INFO

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

PERL5LIB="/Users/llimllib/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/llimllib/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/llimllib/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/llimllib/perl5"; export PERL_MM_OPT;

# install base16 colorscheme
BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
