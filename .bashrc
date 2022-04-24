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
export EDITOR='vim'
export VISUAL="$EDITOR"
export ALTERNATE_EDITOR='vi'

# mail address
export EMAIL="$(cat ~/.email_address)"

export PAGER=less



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

BGREEN='\[\e[0;92m\]'
GREEN='\[\e[0;32m\]'
BRED='\[\e[0;91m\]'
RED='\[\e[0;31m\]'
BBLUE='\[\e[0;94m\]'
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
    xterm*|rxvt*)

        PROMPT_COMMAND='echo -ne "\e]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~} \a"'
        ;;
esac

#==============================================================================
# History
#==============================================================================

unset HISTFILESIZE
export HISTSIZE=1000000
export HISTFILESIZE=1000000

# don't put duplicate lines in the history. See bash(1) for more options
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoredups

# append rather than overwrite command history
shopt -s histappend

# make multi-line commands into single-line ones
shopt -s cmdhist

# show time stamp when displaying history
HISTTIMEFORMAT='%F %T '

# ignore super common commands
HISTIGNORE='ls:bg:fg:history'

# append rather than overwriting history
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
# Aliases and convenience functions
#==============================================================================

alias ls='ls -H --group-directories-first'

# enable color support of ls et al
if [ -x /bin/dircolors ] || [ -x /usr/bin/dircolors ] || [ -x /it/sw/gnutools/bin/dircolors ]; then
    eval $(dircolors -b)
    [ -e "$HOME/.dircolors" ] && eval $(dircolors -b $HOME/.dircolors)

    alias ls='ls --color=auto -H --group-directories-first'
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
alias mv='mv -iv'
alias cp='cp -iv'
alias du='du -sh'

# discourage usage of rm, instead use trash AKA tm
function rm_replacement {
    echo "Stop using rm, stupid!"
    return 1
}
alias tm='trash'
alias rm='rm_replacement'

function cleandir {
    if [[ $# -ne 1 ]] ; then
        echo 'usage: ${0} <dir>'
        return 1
    fi
    local dir="${1}"

    if [ ! -d "${dir}" ] ; then
        #echo "No existing directory ${dir}"
        return 0
    fi
    (
    cd "${dir}"

    dot_files=(.[^.]*)
    files=(*)
    if [ "${dot_files[0]}" != ".[^.]*" ]; then
        trash "${dot_files[@]}"
    fi
    if [ "${files[0]}" != "*" ]; then
        trash "${files[@]}"
    fi
    )

    return 0
}


if [ `uname -o` == "Msys" ]
then
    alias trash="rm -rf"
fi

# make with multiple threads
alias mk='make -j $((2+$(nproc)))'
alias nj='ninja'

dus () {
    \du -sh "$@" | sort -h
}

dusc () {
    local dusclist="$(\du -shc "$@")"
    echo "${dusclist}" | head -n-1 | sort -h
    echo "${dusclist}" | tail -n 1
}

fix-perms ()  {
    find "$1" -type d -exec chmod 755 {} \;
    find "$1" -type f -exec chmod 644 {} \;
}

fix-perms-restr ()  {
    find "$1" -type d -exec chmod 700 {} \;
    find "$1" -type f -exec chmod 600 {} \;
}

bup () {
    dstr=`date +"%Y%m%d%H%S%M"`
    cp "$1" "${1}.${dstr}"
}

success() {
    if [ $? -eq 0 ]; then
        echo "Success"
    else
        echo "Failure"
    fi
}

# archive extraction and listing
extract() {
    if [ $# -ne 1 ]; then
        echo "error: no input argument"
        return 1
    elif [ -f "$1" ]; then
        case $1 in
            *.tar) tar xvf  "$1" ;;
            *.tar.gz|*.tgz) tar xvzf "$1";;
            *.tar.xz|*.txz) tar xvJf  "$1";;
            *.tar.bz2) tar xvjf  "$1";;
            *.tar.bz) tar xvjf  "$1";;
            *.zip) unzip  "$1" ;;
            *.rar) unrar x "$1" ;;
            *.gz) gunzip "$1" ;;
            *.7z) 7z x "$1" ;;
            *) echo "error: file '$1' cannot be extracted" ;;
        esac
    else
        echo "error: file '$1' doesn't exist"
        return 1
    fi
    return 0
}

exls() {
    if [ $# -ne 1 ]; then
        echo "error: no input argument"
        return 1
    elif [ -f "$1" ]; then
        case $1 in
            *.tar) tar tvf "$1" ;;
            *.tar.gz|*.tgz) tar tvzf "$1";;
            *.tar.xz|*.txz) tar tvJf "$1";;
            *.tar.bz2) tar tvjf "$1";;
            *.zip) unzip -l "$1" ;;
            *.rar) unrar l "$1" ;;
            *.7z) 7z l "$1" ;;
            *) echo "error: file '$1' cannot be listed" ;;
        esac
    else
        echo "error: file '$1' doesn't exist"
        return 1
    fi
    return 0
}

alias hex="printf '0x%x\n'"

function hexcalc {
    printf '0x%x\n' $(calc "$*")
}

#==============================================================================
# build stuff with cmake
#==============================================================================

function cbuild {

    local options=$(getopt -o ha:g:t:c:b: -l help,generator:,target:,toolchain:,config:,buildtype: -n "$0" -- "$@")
    eval set -- "$options"

    #now all short and long options are first
    while [[ $1 != -- ]]; do
        case $1 in
            -a|--target)
                local target="$2"
                shift 2
                ;;
            -g|--generator)
                local generator="$2"
                shift 2
                ;;
            -t|--toolchain)
                local toolchain="$2"
                shift 2
                ;;
            -c|--config)
                local config="$2"
                shift 2
                ;;
            -b|--buildtype)
                local buildtype="$2"
                shift 2
                ;;
            -h|--help)
                echo "Usage: $FUNCNAME [options] srcdir builddir cmds [-- cmake-extra-args]"
                echo
                echo "Builds a cmake project with source root at <srcdir>, build dir at <builddir>,"
                echo "according to the commands <cmds> and the options <options>."
                echo
                echo "Commands is a string containing any of the following command letters:"
                echo " * c - removes the build dir if exists"
                echo " * g - invokes cmake generate"
                echo " * b - invokes cmake build"
                echo
                echo "Available options:"
                echo "  -g|--generator      The cmake generator to use"
                echo "  -t|--toolchain      Path to toolchain file"
                echo "  -c|--config         Path to initial cache config file"
                echo "  -b|--buildtype      Cmake build type (existing shorthands: rel=Release,"
                echo "                      dbg|deb=Debug, rwd=RelWithDebInfo)"
                echo "  -a|--target         The cmake target to build"
                echo
                echo "The optional <cmake-extra-args> are passed as-is to the cmake generate step"
                echo
                echo "Example usage:"
                echo "  cbuild . build-21569 cgb -g Ninja -t toolchain/sharc-plus-21569-emulator.cmake -b rwd \\"
                echo "                           -a simple_process -- -DDDSP_ENABLE_PROFILING_CPU=ON"
                return 0
                ;;
            *)
                >&2 echo "bad option: $1"
                return 1
                ;;
        esac
    done
    # remove trailing --
    shift

    # now all the remaining options are positional and extra arguments

    if [ $# -lt 3 ]; then
        >&2 echo 'too few arguments!'
        return 1
    fi

    local sourcedir="$(realpath "$1")"
    local builddir="$(realpath "$2")"
    local cmds="$3"
    shift 3

    if [ $# -gt 0 ]; then
        local extra_args_for_cmake=$@
    fi

    echo "Settings:"
    echo "  sourcedir: $sourcedir"
    echo "  builddir:  $builddir"
    echo "  cmds:      $cmds"
    echo "  generator: $generator"
    echo "  target:    $target"
    echo "  toolchain: $toolchain"
    echo "  config:    $config"
    echo "  buildtype: $buildtype"
    echo "  cmakeargs: $extra_args_for_cmake"

    if [[ $cmds =~ "c" ]] && [[ -e "${builddir}" ]]; then
        echo "Removing existing build dir ${builddir}"
        trash "${builddir}"
    fi

    if [[ $cmds =~ "g" ]]; then
        if [ -n "$toolchain" ]; then
            local toolchain_arg="-DCMAKE_TOOLCHAIN_FILE=$toolchain"
        fi

        if [ -n "$config" ]; then
            local config_arg="-C $config"
        fi

        if [ -n "$generator" ]; then
            local generator_arg="-G $generator"
        fi

        if [ -n "$buildtype" ]; then
            case $buildtype in
                rel)
                    local buildtype=Release
                    ;;
                dbg|deb)
                    local buildtype=Debug
                    ;;
                rwd)
                    local buildtype=RelWithDebInfo
                    ;;
            esac
            local buildtype_arg="-DCMAKE_BUILD_TYPE=$buildtype"
        fi

        cmake "${generator_arg}" -S "${sourcedir}" -B "${builddir}" ${buildtype_arg} ${toolchain_arg} ${config_arg} ${extra_args_for_cmake}
    fi

    if [[ $cmds =~ "b" ]]; then
        if [ -n "$target" ]; then
            local target_arg="--target $target"
        fi
        cmake --build "${builddir}" $target_arg
    fi

    return 0
}

wo () {
    start "`cygpath -w $1`"
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
alias bmake='make -B'
alias rinja='ninja clean && ninja'

# editors
alias v='vim'
alias emx='emacs -nw'
alias e='emacs -nw'
alias smx='sudo emacs -nw'
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
alias xo='xdg-open'

# apt commands
alias ainstall='sudo apt install'
alias apurge='sudo apt purge'
alias aremove='sudo apt remove'
alias aclean='sudo apt autoremove --purge && sudo apt-get autoclean'
alias aupgrade='sudo apt full-upgrade'
alias aupdate='sudo apt update'
alias asearch='apt-cache search'
alias ashow='apt-cache show'
alias ashowpkg='apt-cache showpkg'
alias apolicy='apt-cache policy'

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
alias sshx='ssh -X -C'
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
alias octave='octave --no-gui'

# networking
alias ipca='ip -c a'

# Enable coloring of gcc output
export GCC_COLORS=1
# Enable coloring of gtest output
export GTEST_COLOR=1

# Enable Ninja as default cmake generator
export CMAKE_GENERATOR=Ninja

# for golang
export GOPATH=$HOME/.local/go
export PATH=$PATH:$(go env GOPATH)/bin
