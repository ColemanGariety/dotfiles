#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias startx='startx -- -ardelay 200 -arinterval 30'
alias emacs='emacsclient -nw -c -a ""'
alias edit='emacsclient -nw -c -a ""'

# Fzf
export FZF_DEFAULT_COMMAND="fd -H --color=never"

# Golang
export GOPATH="/home/coleman/Git/go/"

# DynamoDB
export DYNAMODB_ENDPOINT=http://localhost:8080/

# # Xorg
# if [[ -z $DISPLAY ]]; then
#   startx -- -ardelay 200 -arinterval 30
# fi

# TERM
# TERM=xterm

# git
source ~/.git-prompt.sh

# Syntactic sugar for ANSI escape sequences
txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
badgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset

# Prompt variables
PROMPT_BEFORE="\[$txtcyn\]\u@\h \[$txtylw\]\w\[$txtrst\]"
PROMPT_AFTER=" \\\$ "

# Prompt command
PROMPT_COMMAND='__git_ps1 "$PROMPT_BEFORE" "$PROMPT_AFTER"'

# Git prompt features (read ~/.git-prompt.sh for reference)
export GIT_PS1_SHOWDIRTYSTATE="true"
export GIT_PS1_SHOWSTASHSTATE="true"
export GIT_PS1_SHOWUNTRACKEDFILES="true"
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWCOLORHINTS="true"


[ -f ~/.fzf.bash ] && source ~/.fzf.bash
source /usr/share/nvm/init-nvm.sh

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /home/coleman/Git/content-camel/services/node_modules/tabtab/.completions/serverless.bash ] && . /home/coleman/Git/content-camel/services/node_modules/tabtab/.completions/serverless.bash
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /home/coleman/Git/content-camel/services/node_modules/tabtab/.completions/sls.bash ] && . /home/coleman/Git/content-camel/services/node_modules/tabtab/.completions/sls.bash
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[ -f /home/coleman/Git/content-camel/services/node_modules/tabtab/.completions/slss.bash ] && . /home/coleman/Git/content-camel/services/node_modules/tabtab/.completions/slss.bash