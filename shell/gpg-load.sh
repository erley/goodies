#!/bin/sh

export GPG_TTY=$(tty)

if [ ! -f "~/.gpg-agent-info" ]; then
    eval $(gpg-agent --daemon --enable-ssh-support --disable-scdaemon --write-env-file)
else
    . "~/.gpg-agent-info"
fi
export GPG_AGENT_INFO
export SSH_AUTH_SOCK
export SSH_AGENT_PID

#GPG_ENV=~/.gnupg/environment
#
#function start_gpg_agent {
#    echo "Initializing new GPG agent..."
#    (umask 066; gpg-agent --daemon \
#        --allow-emacs-pinentry \
#        --enable-ssh-support \
#        --disable-scdaemon > ${GPG_ENV})
#    . ${GPG_ENV} > /dev/null
#    ssh-add ~/.ssh/id_rsa
#    ssh-add ~/.ssh/muc/muc_id_rsa
#}
#if (ps -C gpg-agent --no-headers -o command | grep "gpg-agent --supervised" > /dev/null) ; then
#    kill -SIGTERM $(ps -C gpg-agent --no-headers -o pid | head -n1)
#    rm -f /var/run/user/$(id -u)/gnupg/*
#fi
#if (ps -C gpg-agent --no-headers -o pid > /dev/null) ; then
#    . ${GPG_ENV} > /dev/null
#else
#    start_gpg_agent
#fi

######################
# Run GPG-Agent daemon
###

#export GPG_TTY=$(tty)
#
#if [ ! -f "~/.gpg-agent-info" ]; then
#    eval $(gpg-agent --daemon --enable-ssh-support --disable-scdaemon --write-env-file)
#else
#    . "~/.gpg-agent-info"
#fi
#export GPG_AGENT_INFO
#export SSH_AUTH_SOCK
#export SSH_AGENT_PID
#############
#gpg-connect-agent updatestartuptty /bye

#if (ps -C gpg-agent --no-headers -o command | grep "gpg-agent --supervised" > /dev/null) ; then
##    kill -SIGHUP $(ps -C gpg-agent --no-headers -o pid | head -n1)
#    echo "GPG-Agent: reconfigure to enable SSH"
#    GPG_AGENT_PID=$(ps -C gpg-agent --no-headers -o pid | head -n1)
#    # do it 3 times according to man page
#    kill -SIGTERM ${GPG_AGENT_PID}
#    kill -SIGTERM ${GPG_AGENT_PID}
#    kill -SIGTERM ${GPG_AGENT_PID}
#    rm -f /var/run/user/$(id -u)/gnupg/*
#fi
#if (ps -C gpg-agent --no-headers -o pid > /dev/null) ; then
#    echo "GPG-Agent: already re-configured"
#else
#    echo "GPG-Agent: initialization"
#    gpg-agent --daemon \
#        --allow-emacs-pinentry \
#        --enable-ssh-support \
#        --disable-scdaemon > /dev/null
##    gpg-connect-agent updatestartuptty /bye
#fi

#if (ps -C gpg-agent --no-headers -o pid > /dev/null) ; then
#    . ${GPG_ENV} > /dev/null
#else
#    echo "Initializing new GPG agent..."
#    (umask 066; gpg-agent --daemon \
#        --allow-emacs-pinentry \
#        --enable-ssh-support \
#        --disable-scdaemon > ${GPG_ENV})
#    . ${GPG_ENV} > /dev/null
#    ssh-add ~/.ssh/id_rsa
#    ssh-add ~/.ssh/muc/muc_id_rsa
#    gpg-connect-agent updatestartuptty /bye
#fi

#if [ "${SSH_AUTH_SOCK:-0}" -ne $$ ]; then
#export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
#fi
#ssh-add ~/.ssh/id_rsa
#ssh-add ~/.ssh/muc/muc_id_rsa
