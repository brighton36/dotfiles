# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

#SH Options
shopt -s histappend
shopt -s checkwinsize

# make less more friendly for non-text input files
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Xterm/gterm prompt mods:
if [ "$TERM" != "linux" ]; then
  export PS1='${debian_chroot:+($debian_chroot)}\[\033[0;31m\]\u\[\033[00m\]@\[\033[0;32m\]\h\[\033[00m\]:\[\033[0;34m\]\w\[\033[00m\]\$ '
  export  PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}:${PWD/#$HOME/~}\007" '
fi

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Dir colors:
# enable color support of ls and grep
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# History stuff:
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT="%h/%d - %H:%M:%S "
export HISTSIZE=20000

# some ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

DEBEMAIL="chris@chrisderose.com"
DEBFULLNAME="Chris DeRose"
export DEBEMAIL DEBFULLNAME

export VISUAL='vim'

export PATH="$PATH:~/bin"

# OSX stuff:
if [ $(uname) = "Darwin" ]; then
  export PATH="$HOME/.asdf/shims:$PATH"
fi
