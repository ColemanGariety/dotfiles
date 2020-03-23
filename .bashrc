#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias ls='ls -F --color=auto'
alias la='ls -Falh --color=auto'
alias hg='history | grep '
alias trash='mv -t ~/.local/share/Trash/files --backup=t'
alias ..='cd ..'
alias ...='cd ../../'
alias ....='cd ../../../'
alias untar='tar -zxvf '
alias startx='startx -- -ardelay 200 -arinterval 30'
alias emacs='emacsclient -nw -a "" -t'
alias e='emacs'
alias chromium-debug='chromium --remote-debugging-port=9222'

# exports
export GOPATH="/home/coleman/Git/go/"
export DYNAMO_ENDPOINT="http://localhost:8000"

# path
PATH=$PATH:$HOME/.cargo/bin
PATH=$PATH:$HOME/.yarn/bin
PATH=$PATH:$HOME/.bin

# TERM
# TERM=xterm-256color

# sudo completion
complete -cf sudo

# git completion
if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi

# nvm
source /usr/share/nvm/init-nvm.sh

# opam
test -r /home/coleman/.opam/opam-init/init.sh && . /home/coleman/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# gitstatus
source ~/.prompt.sh
