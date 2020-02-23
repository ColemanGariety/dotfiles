# Simple Bash prompt with Git status.

# Source gitstatus.plugin.sh from $GITSTATUS_DIR or from the same directory
# in which the current script resides if the variable isn't set.
source /home/coleman/.gitstatus/gitstatus.plugin.sh || return

# Sets GITSTATUS_PROMPT to reflect the state of the current git repository.
# The value is empty if not in a git repository. Forwards all arguments to
# gitstatus_query.
#
# Example value of GITSTATUS_PROMPT: master ⇣42⇡42 ⇠42⇢42 *42 merge ~42 +42 !42 ?42
#
#   master  current branch
#      ⇣42  local branch is 42 commits behind the remote
#      ⇡42  local branch is 42 commits ahead of the remote
#      ⇠42  local branch is 42 commits behind the push remote
#      ⇢42  local branch is 42 commits ahead of the push remote
#      *42  42 stashes
#    merge  merge in progress
#      ~42  42 merge conflicts
#      +42  42 staged changes
#      !42  42 unstaged changes
#      ?42  42 untracked files

# # Syntactic sugar for ANSI escape sequences
# txtblk='\e[0;30m' # Black - Regular
# txtred='\e[0;31m' # Red
# txtgrn='\e[0;32m' # Green
# txtylw='\e[0;33m' # Yellow
# txtblu='\e[0;34m' # Blue
# txtpur='\e[0;35m' # Purple
# txtcyn='\e[0;36m' # Cyan
# txtwht='\e[0;37m' # White
# bldblk='\e[1;30m' # Black - Bold
# bldred='\e[1;31m' # Red
# bldgrn='\e[1;32m' # Green
# bldylw='\e[1;33m' # Yellow
# bldblu='\e[1;34m' # Blue
# bldpur='\e[1;35m' # Purple
# bldcyn='\e[1;36m' # Cyan
# bldwht='\e[1;37m' # White
# unkblk='\e[4;30m' # Black - Underline
# undred='\e[4;31m' # Red
# undgrn='\e[4;32m' # Green
# undylw='\e[4;33m' # Yellow
# undblu='\e[4;34m' # Blue
# undpur='\e[4;35m' # Purple
# undcyn='\e[4;36m' # Cyan
# undwht='\e[4;37m' # White
# bakblk='\e[40m'   # Black - Background
# bakred='\e[41m'   # Red
# badgrn='\e[42m'   # Green
# bakylw='\e[43m'   # Yellow
# bakblu='\e[44m'   # Blue
# bakpur='\e[45m'   # bakcyn='\e[46m'   # Cyan
# bakwht='\e[47m'   # White
# txtrst='\e[0m'    # Text Reset


function gitstatus_prompt_update() {
  GITSTATUS_PROMPT=""

  gitstatus_query "$@"                  || return 1  # error
  [[ "$VCS_STATUS_RESULT" == ok-sync ]] || return 0  # not a git repo

  local      reset=$'\e[0m'         # no color
  local      clean=$'\e[0;32m'  # green foreground
  local  untracked=$'\e[0;36m'  # teal foreground
  local   modified=$'\e[0;33m'  # yellow foreground
  local conflicted=$'\e[1;31m'  # red foreground

  local p

  local where  # branch name, tag or commit
  if [[ -n "$VCS_STATUS_LOCAL_BRANCH" ]]; then
    where="$VCS_STATUS_LOCAL_BRANCH"
  elif [[ -n "$VCS_STATUS_TAG" ]]; then
    p+="${reset}#"
    where="$VCS_STATUS_TAG"
  else
    p+="${reset}@"
    where="${VCS_STATUS_COMMIT:0:8}"
  fi

  (( ${#where} > 32 )) && where="${where:0:12}…${where: -12}"  # truncate long branch names and tags
  p+="${clean}(${where})"

  # # ⇣42 if behind the remote.
  # (( VCS_STATUS_COMMITS_BEHIND )) && p+=" ${clean}⇣${VCS_STATUS_COMMITS_BEHIND}"
  # # ⇡42 if ahead of the remote; no leading space if also behind the remote: ⇣42⇡42.
  # (( VCS_STATUS_COMMITS_AHEAD && !VCS_STATUS_COMMITS_BEHIND )) && p+=" "
  # (( VCS_STATUS_COMMITS_AHEAD  )) && p+="${clean}⇡${VCS_STATUS_COMMITS_AHEAD}"
  # # ⇠42 if behind the push remote.
  # (( VCS_STATUS_PUSH_COMMITS_BEHIND )) && p+=" ${clean}⇠${VCS_STATUS_PUSH_COMMITS_BEHIND}"
  # (( VCS_STATUS_PUSH_COMMITS_AHEAD && !VCS_STATUS_PUSH_COMMITS_BEHIND )) && p+=" "
  # # ⇢42 if ahead of the push remote; no leading space if also behind: ⇠42⇢42.
  # (( VCS_STATUS_PUSH_COMMITS_AHEAD  )) && p+="${clean}⇢${VCS_STATUS_PUSH_COMMITS_AHEAD}"
  # # *42 if have stashes.
  # (( VCS_STATUS_STASHES        )) && p+=" ${clean}*${VCS_STATUS_STASHES}"
  # 'merge' if the repo is in an unusual state.
  [[ -n "$VCS_STATUS_ACTION"   ]] && p+=" ${conflicted}${VCS_STATUS_ACTION}"
  # ~42 if have merge conflicts.
  (( VCS_STATUS_NUM_CONFLICTED )) && p+=" ${conflicted}~${VCS_STATUS_NUM_CONFLICTED}"
  # +42 if have staged changes.
  (( VCS_STATUS_NUM_STAGED     )) && p+=" ${modified}+${VCS_STATUS_NUM_STAGED}"
  # !42 if have unstaged changes.
  (( VCS_STATUS_NUM_UNSTAGED   )) && p+=" ${modified}!${VCS_STATUS_NUM_UNSTAGED}"
  # ?42 if have untracked files. It's really a question mark, your font isn't broken.
  (( VCS_STATUS_NUM_UNTRACKED  )) && p+=" ${untracked}?${VCS_STATUS_NUM_UNTRACKED}"

  GITSTATUS_PROMPT="${p}${reset}"
}

# Start gitstatusd in the background.
gitstatus_stop && gitstatus_start -s -1 -u -1 -c -1 -d -1

# On every prompt, fetch git status and set GITSTATUS_PROMPT.
PROMPT_COMMAND=gitstatus_prompt_update

# Enable promptvars so that ${GITSTATUS_PROMPT} in PS1 is expanded.
# shopt -s promptvars

# Customize prompt. Put $GITSTATUS_PROMPT in it reflect git status.
#
# Example:
#
#   user@host ~/projects/skynet master+!
#   $ █
PS1='\[\033[0m\]\u\[\033[00m\] '           # green user@host
PS1+='\[\033[0;34m\]\w\[\033[00m\]'             # blue current working directory
PS1+='${GITSTATUS_PROMPT:+ $GITSTATUS_PROMPT} '    # git status (requires promptvars option)
PS1+='$ '    # green/red (success/error) $/# (normal/root)
PS1+='\[\e]0;\u@\h: \w\a\]'                       # terminal title: user@host: dir
