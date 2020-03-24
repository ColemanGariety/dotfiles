# The following lines were added by compinstall

# custom

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

# NOTE: we only update the prompt when 1) git is run or 2) the directory is
# changed. It is completely insane to run 'git status' every time you hit the
# 'Enter' key. All those waster CPU cycles...

chpwd_functions=(${chpwd_functions[@]} "gitstatus_prompt_update")

function git() {
    /usr/bin/git $@
    gitstatus_prompt_update
}

export GOPATH="/home/coleman/Git/go/"
export DYNAMO_ENDPOINT="http://localhost:8000"
export GITSTATUS_LOG_LEVEL=DEBUG

PATH=$PATH:$HOME/.cargo/bin
PATH=$PATH:$HOME/.yarn/bin
PATH=$HOME/.bin:$PATH

source /usr/share/nvm/init-nvm.sh
source ~/.zsh/gitstatus.prompt.zsh
PROMPT='%~ $ '               # left prompt: directory followed by %/# (normal/root)
RPROMPT='$GITSTATUS_PROMPT' 

gitstatus_prompt_update
