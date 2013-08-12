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

# mail address
export EMAIL="k.ljungkvist@gmail.com"

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
# KL: /etc/profile sources /etc/profile.bash which sources /etc/bash.bashrc which does this.
# KL: No, that's not the case. Sopa.
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
    # *UTF-8|*utf8)
    #   PS1="┌[\h \w]\n└${USER_COLOR}\u${NORMAL} \$ "
    #   ;;
    # *)
    #   # without funny characters
    #   PS1="|[\h \w]\n|${USER_COLOR}\u${NORMAL} \$ "
    #   ;;
    *)
        PS1="[${USER_COLOR}\u${NORMAL}@\h \W]\$ "
#       PS1="[\u@\h \W]\$ "
        ;;
esac

# unset color_prompt force_color_prompt

##### Sets the prompt command. Updates title
case "$TERM" in
    xterm*|rxvt*|screen*)

        PROMPT_COMMAND='echo -ne "\e]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~} \a"'
        ;;
esac

PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'history -a'

#==============================================================================
# Greeting
#==============================================================================

# printHeader () {

#     local hour=$(date +%H)
#     local greeting="Good night"

#     if [ $hour -ge 6 ] && [ $hour -lt 9 ]; then
#       greeting="Good morning"
#     elif [ $hour -ge 9 ] && [ $hour -lt 18 ]; then
#       greeting="Hello"
#     elif [ $hour -ge 18 ]; then
#       greeting="Good evening"
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
alias lls='ls -lhSr'
alias llas='ls -lhASr'

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
    \du -sh "$@" | sort -h
}


fix-perms ()  {
    find "$1" -type d -exec chmod 755 {} \;
    find "$1" -type f -exec chmod 644 {} \;
}

fix-perms-restr ()  {
    find "$1" -type d -exec chmod 700 {} \;
    find "$1" -type f -exec chmod 600 {} \;
}


# Wraps a completion function
# make-completion-wrapper <actual completion function> <name of new func.> <alias>
#                         <command name> <list supplied arguments>
# eg.
# alias agi='apt-get install'
# make-completion-wrapper _apt_get _apt_get_install apt-get install
# defines a function called _apt_get_install (that's $2) that will complete
# the 'agi' alias. (complete -F _apt_get_install agi)
#
#make-completion-wrapper _git_checkout _git_checkout_shortcut go git checkout
make-completion-wrapper () {
    local comp_function_name="$1"
    local function_name="$2"
    local alias_name="$3"
    local arg_count=$(($#-4))
    shift 3
    local args="$*"
    local function="
function $function_name {
    COMP_LINE=\"$@\${COMP_LINE#$alias_name}\"
    let COMP_POINT+=$((${#args}-${#alias_name}))
    ((COMP_CWORD+=$arg_count))
    COMP_WORDS=("$@" \"\${COMP_WORDS[@]:1}\")

    local cur words cword prev
    _get_comp_words_by_ref -n =: cur words cword prev
    "$comp_function_name"
    return 0
}"
    eval "$function"
}

# system information
alias df='df -h'
alias psgrep='ps aux | grep -i'
alias rl='readlink -f'

# programming etc
alias rmake='make clean && make'

# editors
alias v='vim'
alias emx='emacs -nw'
alias e='emacs -nw'
alias smx='sudo vim'
alias svi='sudo vim'
alias emc="emacsclient -c -n -a ''"
alias emt="emacsclient -t -a ''"
alias emd='emacs --daemon'
alias ediff='emacs --diff'

alias svim='sudo vim'

# git
alias g='git'
complete -F _git g
alias gitfind='git ls-tree -r HEAD | grep -i'
alias gitk='gitk --all'

# less/man in separate window
alias xless="xterm -e less"
alias xman="xterm -e man"


# various abrevations/shorcuts
alias hemsida='lftp -u kalj0193 home.student.uu.se'
alias ciplogin='ssh kalle@ciplogin.physik.uni-freiburg.de'
alias bashrc="$EDITOR ~/.bashrc"
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
alias ashowpkg='apt-cache showpkg'

alias dpgrep='dpkg -l | grep'

# completion of apt aliases
[ ! -f /usr/share/bash-completion/completions/apt-cache ] || source /usr/share/bash-completion/completions/apt-cache
[ ! -f /usr/share/bash-completion/completions/apt-get ] || source /usr/share/bash-completion/completions/apt-get
make-completion-wrapper _apt_get _ainstall ainstall apt-get install
complete -F _ainstall ainstall
make-completion-wrapper _apt_get _apurge apurge apt-get purge
complete -F _apurge apurge
make-completion-wrapper _apt_cache _asearch asearch apt-cache search
complete -F _asearch asearch
make-completion-wrapper _apt_cache _ashow ashow apt-cache show
complete -F _ashow ashow
make-completion-wrapper _apt_cache _ashowpkg ashowpkg apt-cache showpkg
complete -F _ashowpkg ashowpkg

# pacman
alias supd='sudo pacman -Syu'

# ssh
alias sshx='ssh -c arcfour,blowfish-cbc -X -C'
complete -F _ssh sshx

# Transmission BitTorrent client
alias torr='transmission-remote'

# shortcuts to openoffice.org
alias oocalc='soffice -calc'
alias oowrite='soffice -writer'

alias locate='locate -i'

# matlab: remove splash screen at startup
alias matlab='matlab -nosplash'
alias mlab='matlab -nodesktop'
alias octave='octave -q'
