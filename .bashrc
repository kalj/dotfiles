# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set standard text editor: emacs!
export EDITOR='emacs -nw'
export VISUAL="$EDITOR"
export ALTERNATE_EDITOR='\emacs -q'

# change directory path variable
export CDPATH=.:..

export PAGER=less

# don't put duplicate lines in the history. See bash(1) for more options
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoredups

# append rather than overwrite command history
shopt -s histappend

export HISTSIZE=1000

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


#==============================================================================
# prompt
#==============================================================================

BGREEN='\[\e[1;32m\]'
GREEN='\[\e[0;32m\]'
BRED='\[\e[1;31m\]'
RED='\[\e[0;31m\]'
BBLUE='\[\e[1;34m\]'
BLUE='\[\e[0;34m\]'
NORMAL='\[\e[00m\]'

# set user color
case "$TERM" in
    xterm*|linux|screen*|rxvt*|cygwin|dumb|*-color)
	if [ ${UID} -eq 0 ]; then
	    USER_COLOR=$BRED
	else
	    USER_COLOR=$BBLUE
	fi
	;;
    *)
	USER_COLOR=""
	;;
esac

# if unicode, set a fancy prompt 
case "$LANG" in
    *UTF-8|*utf8)
    	PS1="┌[\h \w]\n└${USER_COLOR}\u${NORMAL} \$ "
    	;;
    *)
	# without funny characters
	PS1="|[\h \w]\n|${USER_COLOR}\u${NORMAL} \$ "
	;;
    # *)
    # 	# fail-safe
    # 	PS1="|-\h-> \w\n|-\u \#\$ "
    # 	;;
esac

# unset color_prompt force_color_prompt

# Minimal prompt
# PS1='[\u@\h \W]\$ '


##### Sets the prompt command. Updates title
case "$TERM" in
    xterm*|rxvt*|screen*)

	PROMPT_COMMAND='echo -ne "\e]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~} \a"; history -a'
	;;
    *)
	PROMPT_COMMAND='history -a'
        ;;
esac


#==============================================================================
# Greeting
#==============================================================================

# printHeader () {

#     local hour=$(date +%H)
#     local greeting="Good night"

#     if [ $hour -ge 6 ] && [ $hour -lt 9 ]; then
# 	greeting="Good morning"
#     elif [ $hour -ge 9 ] && [ $hour -lt 18 ]; then
# 	greeting="Hello"
#     elif [ $hour -ge 18 ]; then
# 	greeting="Good evening"
#     fi

#     local idag=$(date +"%A %F")
#     local tid=$(date +%T)
#     echo -e "$greeting user $USER.\n\nToday is $idag.\n"
#     ~/bin/upt 1
#     echo -e ""

# }

#==============================================================================
# Aliases
#==============================================================================

alias ls='ls --group-directories-first'

# enable color support of ls et al
if [ -x /bin/dircolors ] || [ -x /usr/bin/dircolors ] || [ -x /it/sw/gnutools/bin/dircolors ]; then
    eval $(dircolors -b)    
    [ -e "$HOME/.dircolors" ] && eval $(dircolors -b $HOME/.dircolors)
    
    alias ls='ls --color=auto --group-directories-first'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias rgrep='rgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# shell navigation etc.
alias sl='ls'
alias l='ls -C'
alias ll='ls -lh'
alias la='ls -A'
alias lla='ls -lhA'
alias lal='ls -lhA'
alias llt='ls -lhtr'
alias llat='ls -lhAtr'

alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# file commands
alias rm='rm -v'
alias mv='mv -iv'
alias cp='cp -iv'
alias du='du -sh'
alias cleanup='rm -f *~ *# .*~'


dus () {
    \du -sk "$@" |
    sort -rn |
    awk -F '\t' '{
if ($1 < 1024) printf "%s\t%s\n", $1 "K", $2
else if ($1 < 1024^2) printf "%3.1f%s\t%s\n", $1/1024, "M", $2
else if ($1 < 1024^3) printf "%3.1f%s\t%s\n", $1/1024^2, "G", $2
else if ($1 < 1024^4) printf "%3.1f%s\t%s\n", $1/1024^3, "T", $2
else printf "%.1f%s\t%s\n", $1/1024^4, "P", $2
}'

}

dus2 () {

    \du -sh "$@" | 
    while read row; do
	echo "$row" | awk '{
len=length($1)
size=substr($1,0,len-1)
suf=substr($1,len,1)
if(suf=="K") printf "%d\t%s\n", int(size), $0
else if(suf=="M") printf "%d\t%s\n", int(size*1024), $0
else if(suf=="G") printf "%d\t%s\n", int(size*1024^2), $0
else if(suf=="T") printf "%d\t%s\n", int(size*1024^3), $0
else if(suf=="P") printf "%d\t%s\n", int(size*1024^4), $0
else print "error reading suffix"
}'
    done |
    sort -rn |
    cut -f2,3
}


fix-perms ()  {
    find "$1" -type d -exec chmod 755 {} \;
    find "$1" -type f -exec chmod 644 {} \;
}

fix-perms-restr ()  {
    find "$1" -type d -exec chmod 700 {} \;
    find "$1" -type f -exec chmod 600 {} \;
}

# system information
alias df='df -h'
alias psgrep='ps aux | grep'

# programming etc
alias rmake='make clean && make'

# emacs
alias emx='emacs -nw'
alias smx='sudo emacs -nw'
alias emc='emacsclient -c -n'
alias emt='emacsclient -t'
alias emd='emacs --daemon'

# various abrevations/shorcuts
alias hemsida='lftp -u kalj0193 home.student.uu.se'
alias ciplogin='ssh kalle@ciplogin.physik.uni-freiburg.de'
alias bashrc="$EDITOR ~/.bashrc"
alias calc='bc -l'
alias go='gnome-open'

# apt commands
alias ainstall='sudo apt-get install'
alias apurge='sudo apt-get purge'
alias aremove='sudo apt-get remove'
alias aclean='sudo apt-get autoremove && sudo apt-get autoclean'
alias aupgrade='sudo apt-get dist-upgrade'
alias aupdate='sudo apt-get update'
alias asearch='apt-cache search'
alias ashow='apt-cache show'
alias dpgrep='dpkg -l | grep'

# pacman
alias supd='sudo pacman -Syu'

# ssh
alias sshx='ssh -c arcfour,blowfish-cbc -X -C'

# Transmission BitTorrent client
alias torr='transmission-remote'

# shortcuts to openoffice.org
alias oocalc='soffice -calc'
alias oowrite='soffice -writer'

# matlab: remove splash screen at startup
# alias matlab='matlab -nosplash'
# alias mlab='matlab -nodesktop'
