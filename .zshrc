# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

###############
# Completions #
###############

# Completions
autoload -Uz compinit
compinit -d ${XDG_CACHE_HOME:-~/.cache}/.zcompdump-$ZSH_VERSION

# Arrow key menu for completions
zstyle ':completion:*' menu select
zstyle ':completion:*'                  matcher-list    'm:{a-zA-Z}={A-Za-z}' 'l:|=* r:|=*'
zstyle ':completion:*:descriptions'     format          '[%d]'
zstyle ':completion:*'                  completer       _complete
zstyle ':completion:*:*:-subscript-:*'  tag-order       indexes parameters
zstyle ':completion:*'                  squeeze-slashes true
zstyle '*'                              single-ignored  show
zstyle ':completion:*:(rm|kill|diff):*' ignore-line     other
zstyle ':completion:*:rm:*'             file-patterns   '*:all-files'
zstyle ':completion::complete:*'        use-cache       on
zstyle ':completion::complete:*'        cache-path      ${XDG_CACHE_HOME:-$HOME/.cache}/zcompcache-$ZSH_VERSION

zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete # completion menu backtab

# Make it possible to use completion specifications and functions written for bash.
autoload -Uz bashcompinit
bashcompinit

###########
# History #
###########

# # number of lines kept in history
# export HISTSIZE=1000
# # number of lines saved in the history after logout
# export SAVEHIST=1000
# # location of history
# export HISTFILE=~/.zsh_history
# # append command to history file once executed
# setopt inc_append_history

########
# Misc #
########

# Automatically use cd when paths are entered without cd
setopt autocd

# Use emacs keybinds, since they're modeless and closer to bash defaults
bindkey -e

###########
# Aliases #
###########

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

setopt completealiases

# fix broken zsh git completion
__git_files () { 
    _wanted files expl 'local files' _files     
}

###########
# Exports #
###########

export GOPATH="/home/coleman/Git/go/"
export DYNAMO_ENDPOINT="http://localhost:8000"
export GITSTATUS_LOG_LEVEL=DEBUG
export EDITOR=e
export BROWSER=chromium

PATH=$PATH:$HOME/.cargo/bin
PATH=$PATH:$HOME/.yarn/bin
PATH=$HOME/.bin:$PATH

#######
# NVM #
#######

source /usr/share/nvm/init-nvm.sh

##########
# Prompt #
##########

ZLE_RPROMPT_INDENT=0
source ~/.zsh/gitstatus.prompt.zsh
PROMPT='%~ $ '               # left prompt: directory followed by %/# (normal/root)
RPROMPT='$GITSTATUS_PROMPT %t'

function precmd() {
    sleep 0;
}
