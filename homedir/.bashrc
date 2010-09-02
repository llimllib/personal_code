# Sample .bashrc for SuSE Linux
# Copyright (c) SuSE GmbH Nuernberg

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
export EDITOR=/usr/local/bin/vim
export TERM=xterm-color

#get git completion script
source ~/.git-completion.bash

#export PROMPT_COMMAND='echo -ne "\033]0;\007"'
function title {
    echo -ne "\033]0;$1\007"
}

alias status='svn status | egrep -v "^Performing status|^$|^X"'
alias ls='ls -FG'
#LS_COLORS='di=36:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:*.rpm=90'
export LSCOLORS=dxfxcxdxbxegedabagacad
export LSCOLORS 

alias sqlite='sqlite3'
PS1='\[\e[36m\]\@\[\e[0m\] \[\e[1;32m\]\w\[\e[0m\]\[\e[0;31m\]$(__git_ps1 " %s")\[\e[0m\]\n\$ '
alias g="git"
alias gs="git status"
alias gd="git diff"
alias gm="git co master"
alias gb="git branch"
alias gc="git checkout"
alias gm="git merge --squash"

alias fc="cd ~/clean-git/FuseController/src/api"
alias scout="cd ~/clean-git/Scout/src/scout/"
alias sc="scout"

alias rack="ack --ruby --follow"
alias fack="ack --actionscript --follow"
alias jack="ack --js --html"
alias pack="ack --python"
alias cack="ack --cc --cpp"
alias aack="ack --all"

alias t="tree"

alias s="sl"
alias l="sl"

alias ls='ls -FG'
PS1="\[\033[1;34m\]\@ \[\033[1;32m\]\w\[\033[0m\]\$ "

export PAGER=less
export CVS_RSH=ssh

export MANPATH=$MANPATH:/opt/local/man

export PATH=$PATH:/usr/local/bin
export SITE_PACKAGES="/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/site-packages/"

shopt -s nocaseglob

[ -f ~/.git-bash-completion.sh ] && . ~/.git-bash-completion.sh

if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

if [[ -s /Users/bill/.rvm/scripts/rvm ]] ; then source /Users/bill/.rvm/scripts/rvm ; fi
