############
# neofetch #
############

# neofetch

#############
# Auto-pair #
#############

source ~/.zsh/zsh-autopair/autopair.zsh
autopair-init

###############
# Completions #
###############

# Completions
# see: https://medium.com/@dannysmith/little-thing-2-speeding-up-zsh-f1860390f92
autoload -Uz compinit 
if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C;
fi;

# compinit -d ${XDG_CACHE_HOME:-~/.cache}/.zcompdump-$ZSH_VERSION

# zstyle ':completion:*' tag-order all-expansions
# Arrow key menu for completions
zstyle ':completion:*:*:*:*:*' menu yes select
zstyle ':completion::complete:*'        use-cache       on
zstyle ':completion::complete:*'        cache-path      ${XDG_CACHE_HOME:-$HOME/.cache}/zcompcache-$ZSH_VERSION

backward-kill-dir () {
    local WORDCHARS=${WORDCHARS/\/}
    zle backward-kill-word
}
bindkey '^[[Z' reverse-menu-complete # completion menu backtab

# Make it possible to use completion specifications and functions written for bash.
# autoload -Uz bashcompinit
# bashcompinit

# plugin
fpath=(~/.zsh/zsh-completions/src $fpath)

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

# backward kill word or directory
autoload -U select-word-style
select-word-style bash

# no autocd
unsetopt autocd

###########
# Aliases #
###########

alias ls='ls -F --color=auto'
alias la='ls -Falh --color=auto'
alias hg='history | grep '
alias trash='mv -t ~/.local/share/Trash/files --backup=t'
alias .='cd .'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias untar='tar -zxvf '
alias startx='startx -- -ardelay 200 -arinterval 30'
alias emacs='emacsclient -nw -a "" -t'

setopt completealiases

# fix broken zsh git completion
__git_files () { 
    _wanted files expl 'local files' _files     
}

#####################
# Colored man pages #
#####################

source ~/.zsh/zsh-colored-man-pages/colored-man-pages.plugin.zsh

#######
# NVM #
#######

source /usr/share/nvm/init-nvm.sh

###################
# Autosuggestions #
###################

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_MANUAL_REBIND=1
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#555"
ZSH_AUTOSUGGEST_STRATEGY="completion"

##########
# Prompt #
##########

ZLE_RPROMPT_INDENT=0
source ~/.zsh/gitstatus.prompt.zsh
PROMPT='%~ $ '               # left prompt: directory followed by %/#
# (normal/root)
# RPROMPT='$GITSTATUS_PROMPT'
RPROMPT='$GITSTATUS_PROMPT%{%F{blue}%}'

function precmd() {
    # set window title
    print -Pn "\e]0;%~\a"
}

#######################
# Syntax Highlighting #
#######################

source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[path]='fg=gray'

# [ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

###########
## PyEnv ##
###########

if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi
