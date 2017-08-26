# ~/.bashrc ou /etc/bash.bashrc : executed by bash(1) for non-login shells.

color_prompt=yes

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines or lines starting with space in the history
#export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

##############
# Prompt color
##############

# get current branch in git repo
function parse_git_branch() {
    BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
    if [ ! "${BRANCH}" == "" ]
    then
        STAT=$(parse_git_dirty)
        echo "[${BRANCH}${STAT}] "
    else
        echo ""
    fi
}

# get current status of git repo
function parse_git_dirty {
    status=$(git status 2>&1 | tee)
    dirty=$(echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?")
#    untracked=$(echo -n "${status}" 2> /dev/null | grep "Untracked files:" &> /dev/null; echo "$?")
    ahead=$(echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?")
    newfile=$(echo -n "echo${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?")
    repofunctionnamed=$(echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?")
    deleted=$(echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?")
    bits=''
    if [ "${renamed}" == "0" ]; then
        bits=">${bits}"
    fi
    if [ "${ahead}" == "0" ]; then
        bits="*${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
        bits="+${bits}"
    fi
#    if [ "${untracked}" == "0" ]; then
#        bits="?${bits}"
#    fi
    if [ "${deleted}" == "0m" ]; then
        bits="x${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
        bits="!${bits}"
    fi
    if [ ! "${bits}" == "" ]; then
        echo " ${bits}"
    else
        echo ""
    fi
}

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color)
        color_prompt=yes
    ;;
esac

if [ "$color_prompt" = yes ]; then
    PS1_1="\[\e[36m\]\t\[\e[m\] "
    PS1_2="\[\e[32m\]\u\[\e[m\]@"
    PS1_3="\[\e[36m\]\h\[\e[m\] "
    PS1_4="\[\e[32m\]\w\[\e[m\] "
    PS1_5="\[\e[33m\]\$(parse_git_branch)\[\e[m\]"
    PS1_6="\[\e[1;31m\]>\[\e[m\] "
    PS1="${PS1_1}${PS1_2}${PS1_3}${PS1_4}${PS1_5}${PS1_6}"
#    PS1="\[\e[0;33m\]\u\[\e[m\]@\[\e[0;36m\]\h\[\e[m\] \[\e[1;34m\]\W\[\e[m\] \[\e[0;33m\]>\[\e[m\] "
    PS2="\[\e[0;30m\]\u\[\e[m\]\[\e[0;35m\]@\[\e[m\]\[\e[0;30m\]\h\[\e[m\] \[\e[0;30m\]\W\[\e[m\] \[\e[0;31m\]>\[\e[m\] "
    PS3="#? "
    PS4="+ % "
else
    PS1='${debian_chroot:+($debian_chroot)}$\u@\h \W \$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    :xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@{HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
    *)
    ;;
esac

export LS_COLORS='ow=1;34:ln=1;34'
export BACKGROUND_COLOR=black # or 'white'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi
if [ "$INSIDE_EMACS" != "" ]; then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
fi

# some more ls aliases
alias lll='ls -lFhXL'
alias llla='ls -lFhXLA'
alias ll='ls -lFhX'
alias lla='ls -lFhXA'
alias l='ls -CF'
alias la='ls -CFA'
alias lf='ls -1F'

alias h='history'
alias j='jobs'

alias weather='curl wttr.in/Antibes'
alias psgrep='ps o pid,user,args -C'
alias gitlog='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%s"'

export JAVA_HOME=$(realpath $(dirname $(readlink -f $(which java)))/../..)

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

export PATH=$PATH:/usr/sbin:$HOME/.local/bin

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'		# begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'	# begin bold
export LESS_TERMCAP_me=$'\E[0m'			# end mode
export LESS_TERMCAP_se=$'\E[0m'			# end standout-mode
export LESS_TERMCAP_so=$'\E[1;31;5;246m'	# begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'			# end underline
export LESS_TERMCAP_us=$'\E[04;33;5;146m'	# begin underline

# Correction automatique des fautes de frappe sur la commande cd
shopt -s cdspell

######################
# Run SSH-Agent daemon
###

#SSH_ENV=~/.ssh/environment
#
#function start_ssh_agent {
#    echo "Initialising new SSH agent..."
#    (umask 066; ssh-agent -s > ${SSH_ENV})
#    . ${SSH_ENV} > /dev/null
#    ssh-add ~/.ssh/id_rsa
#    ssh-add ~/.ssh/muc/muc_id_rsa
#}
#if [ -f ${SSH_ENV} ]; then
#    . ${SSH_ENV} > /dev/null
#    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent > /dev/null || start_ssh_agent
#else
#    start_ssh_agent
#fi

######################
# Run GPG-Agent daemon
###

export GPG_TTY=$(tty)

## on Prometheus laptop you have to
# sudo apt-get install gpg-agent
# sed -i 's/^use-ssh-agent/#use-ssh-agent/' /etc/X11/Xsession.options
# sed -i 's/--daemon --sh/--daemon --enable-ssh-support --disable-scdaemon --sh/' /etc/X11/Xsession.d/90gpg-agent

## on VBox you run the script gpg-load.sh
