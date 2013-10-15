# Sample .bashrc for SuSE Linux
# Copyright (c) SuSE GmbH Nuernberg

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
export EDITOR=/usr/local/bin/vim
export TERM=xterm

#set go root dir
export GOPATH=~/go/

#get git completion script
source ~/.git-completion.bash

#export PROMPT_COMMAND='echo -ne "\033]0;\007"'
function title {
    echo -ne "\033]0;$1\007"
}

function cheflog {
    vagrant ssh $1 -c "tail -f /var/log/chef/client.log"
}

alias tailengine='cheflog engine'

alias status='svn status | egrep -v "^Performing status|^$|^X"'
alias ls='ls -FG'
#LS_COLORS='di=36:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:*.rpm=90'
export LSCOLORS=dxfxcxdxbxegedabagacad
export LSCOLORS

# Fix colors in ipython paging
export PAGER="less"
export LESS="-r -S"

alias sqlite='sqlite3'
PS1='\[\e[36m\]\@\[\e[0m\] \[\e[1;32m\]\H:\w\[\e[0m\] \[\033[01;34m\]$(~/.rvm/bin/rvm-prompt)\[\033[01;32m\]\[\e[0;31m\]$(__git_ps1 " %s")\[\e[0m\] \n\$ '

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
alias gls='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'
alias gll='git log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'
alias prune='git remote prune origin'

alias hs='ls && echo "---------------------------------------" && hg status'
alias hd='hg diff'
alias hm='hg ci -m'

alias rack="ack --ruby --follow"
alias fack="ack --actionscript --follow"
alias jack="ack --js --html"
alias pack="ack --python"
alias cack="ack --cc --cpp"
alias gack="ack --go"
alias aack="ack -k -i"

alias t="tree | less"

alias s="sl"
alias l="sl"

alias ls='ls -FG'

export CVS_RSH=ssh

export MANPATH=$MANPATH:/opt/local/man

export PATH=/usr/local/share/python:/usr/local/bin:/usr/local/Cellar/python/2.7/bin:/usr/local/sbin:/usr/local/share/npm/bin/:$PATH

shopt -s nocaseglob

[ -f ~/.git-bash-completion.sh ] && . ~/.git-bash-completion.sh

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/share/python/virtualenvwrapper.sh

if [[ -s /usr/local/opt/autoenv/activate.sh ]] ; then source /usr/local/opt/autoenv/activate.sh ; fi
if [[ -s ~/.rvm/scripts/rvm ]] ; then source ~/.rvm/scripts/rvm ; fi
if [[ ! -s ~/.rvm/hooks/after_cd ]] || ! grep -q autoenv_init ~/.rvm/hooks/after_cd
then
  echo "#!/usr/bin/env bash
# https://github.com/kennethreitz/autoenv/issues/45
autoenv_init" >> ~/.rvm/hooks/after_cd
fi

if [ -f `brew --prefix`/etc/bash_completion ]; then
. `brew --prefix`/etc/bash_completion
fi

#don't write pyc or pyo files
export PYTHONDONTWRITEBYTECODE=1

#lex
export LEX_DATA=/usr/local/share/lex/data
export LEX_DB=/usr/local/share/lex/db

export GOROOT=/usr/local/Cellar/go/1.1/
