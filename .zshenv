setopt noglobalrcs

###########
# Exports #
###########

export GOPATH="/home/coleman/git/go/"
export DYNAMO_ENDPOINT="http://localhost:8000"
export GITSTATUS_LOG_LEVEL=DEBUG
export EDITOR=e
export BROWSER=chromium

export PYENV_ROOT="$HOME/.pyenv"
export PIPENV_PYTHON="$PYENV_ROOT/shims/python"

# # gonna set this manually
# export AWS_PROFILE=humindex

export LSP_USE_PLISTS=true

PATH=$PATH:$HOME/.cargo/bin
PATH=$PATH:$HOME/.yarn/bin
PATH=$HOME/.bin:$PATH
PATH=$HOME/.local/bin:$PATH
PATH=$PYENV_ROOT/shims:$PATH
PATH=$PATH:/home/coleman/.local/share/gem/ruby/3.0.0/bin

LANG=en_US.UTF-8
LC_ALL=en_US.UTF-8
