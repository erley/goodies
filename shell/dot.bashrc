# ~/.bashrc ou /etc/bash.bashrc : executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#################
# Global settings
###

# don't put duplicate lines or lines starting with space in the history.
#export HISTCONTROL=ignoreboth
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Autocorrection of directory path for cd command
shopt -s cdspell

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
###

# get current branch in git repo
function parse_git_branch() {
    BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
    if [ ! "${BRANCH}" == "" ] ; then
        STAT=$(parse_git_dirty)
        #echo -n $'\xce\x8e ' # 'Y' = \u038e (other codes: \u07c7, \u0427
        echo -n $'\xe2\x8c\xa5 ' # branch symbol = \u2325 (\x2387)
        echo -n "${BRANCH} ${STAT}"
    fi
}

# get current status of git repo
function parse_git_dirty {
    status=$(git status 2>&1 | tee)
    dirty=$(echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?")
    untracked=$(echo -n "${status}" 2> /dev/null | grep "Untracked files:" &> /dev/null; echo "$?")
    ahead=$(echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?")
    newfile=$(echo -n "echo${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?")
    deleted=$(echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?")
    bits=''
    if [ "${ahead}" == "0" ]; then
        bits=$'\xe2\x86\x91'${bits}  # '*' => \xe2\x86\x91
    fi
    if [ "${dirty}" == "0" ]; then
        #bits=$'\xe2\x89\xa0'${bits}  # '!' => "\u2260"
        bits=$'\xe2\x9c\x90'${bits}  # pencil = "\u2710"
    fi
    if [ "${newfile}" == "0" ]; then
        bits="+${bits}"
    fi
    if [ "${untracked}" == "0" ]; then
        bits="?${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
        bits=$'\xe2\x9c\x98'${bits}  # 'x' = "\u2718"
    fi
    if [ ! "${bits}" == "" ]; then
        echo -n ${bits}
    else
        echo -n $'\xe2\x9c\x94'  # 'v' = \u2714
    fi
}

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-256color|screen-256color)
        color_prompt=yes
    ;;
esac
if [ "$INSIDE_EMACS" != "" ]; then
    color_prompt=yes
fi
# show only 2 deepest PWD levels in prompt
export PROMPT_DIRTRIM=2
if [ "$color_prompt" = yes ]; then
    PS1_1="\[\e[36m\e[48;5;236m\]\t\[\e[0m\] "
    PS1_2="\[\e[1;32m\e[4m\]\u\[\e[0m\]\[\e[4m\]@"
    PS1_3="\[\e[1;36m\]\h\[\e[0m\] "
    PS1_4="\[\e[32m\]\w\[\e[0m\] "
    PS1_5="\[\e[36m\e[48;5;236m\]\$(parse_git_branch)\[\e[0m\] "
    PS1_6=$'\\[\e[1;32m\\]\xe2\x96\xb6\\[\e[0m\\] '  # '>' = \u25b6

    PS1="${PS1_1}${PS1_2}${PS1_3}${PS1_4}${PS1_5}${PS1_6}"
    PS2="\[\e[0;30m\]\u\[\e[0m\]\[\e[0;35m\]@\[\e[0m\]\[\e[0;30m\]\h\[\e[0m\] \[\e[0;30m\]\W\[\e[0m\] \[\e[0;31m\]>\[\e[0m\] "
    PS3="#? "
    PS4="+ % "
    #echo -n " " ; for n in {0..9} {a..f} ; do echo -n " ${n}" ; done ; echo ; for i in {0..9} {a..f} ; do echo -n "${i} " ; for n in {0..9} {a..f} ; do echo -e -n "\u33${i}${n} " ; done; echo ; done
else
    PS1='${debian_chroot:+($debian_chroot)}$\u@\h \W \$ '
fi
unset color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    :xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@{HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
    *)
    ;;
esac

#########
# Exports
###

export LS_COLORS='ow=1;34:ln=1;34'
export BACKGROUND_COLOR=black # or 'white'
export JAVA_HOME=$(realpath $(dirname $(readlink -f $(which java)))/..)
export PATH=$PATH:/usr/sbin:$HOME/.local/bin
export PAGER=less

# Less colors for man pages
export LESS_TERMCAP_mb=$'\E[01;31m'		# begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'	# begin bold
export LESS_TERMCAP_me=$'\E[0m'			# end mode
export LESS_TERMCAP_se=$'\E[0m'			# end standout-mode
export LESS_TERMCAP_so=$'\E[1;31;5;246m'	# begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'			# end underline
export LESS_TERMCAP_us=$'\E[04;33;5;146m'	# begin underline

###################
# Alias definitions
###

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "$(dircolors -b)"
    alias ls='ls --color=auto'

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

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

#############################
# Command line autocompletion
###

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -f ~/.bash_completion.oc ]; then
    . ~/.bash_completion.oc
fi

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

## on VBox GPG Agent is initialized this way:
if [ "$HOSTNAME" == "pandora" ]; then
    GPG_ENV=~/.gnupg/environment

    if (ps -C gpg-agent --no-headers -o command | grep "gpg-agent --supervised" > /dev/null) ; then
        echo "GPG-Agent: reconfigure to enable SSH"
        GPG_AGENT_PID=$(ps -C gpg-agent --no-headers -o pid | head -n1)
        # do it 3 times according to man page
        kill -SIGTERM ${GPG_AGENT_PID}
        kill -SIGTERM ${GPG_AGENT_PID}
        kill -SIGTERM ${GPG_AGENT_PID}
        rm -f /var/run/user/$(id -u)/gnupg/*
    fi
    if (ps -C gpg-agent --no-headers -o pid > /dev/null) ; then
        echo "GPG-Agent: already re-configured"
    else
        echo "GPG-Agent: initialization"
        (umask 066; gpg-agent --daemon \
            --allow-emacs-pinentry \
            --enable-ssh-support \
            --disable-scdaemon > ${GPG_ENV})
        . ${GPG_ENV} > /dev/null
#        gpg-connect-agent updatestartuptty /bye
    fi
fi

## on Prometheus laptop you have to
# sudo apt-get install gpg-agent
# sed -i 's/^use-ssh-agent/#use-ssh-agent/' /etc/X11/Xsession.options
# sed -i 's/--daemon --sh/--daemon --enable-ssh-support --disable-scdaemon --sh/' /etc/X11/Xsession.d/90gpg-agent

