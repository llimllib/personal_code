# Sample .bashrc for SuSE Linux
# Copyright (c) SuSE GmbH Nuernberg

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
export EDITOR=/usr/local/bin/vim
export TERM=xterm-color

alias ls='ls -FG'
PS1="\[\033[1;34m\]\@ \[\033[1;32m\]\w\[\033[0m\]\$ "

export PAGER=less
export CVS_RSH=ssh

export MANPATH=$MANPATH:/opt/local/man

export PATH=$PATH:/usr/local/bin
export SITE_PACKAGES="/Library/Frameworks/Python.framework/Versions/2.5/lib/python2.5/site-packages/"

shopt -s nocaseglob
