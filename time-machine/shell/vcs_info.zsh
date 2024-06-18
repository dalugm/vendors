# @see https://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh
# @see https://github.com/zsh-users/zsh/blob/master/Misc/vcs_info-examples

setopt PROMPT_SUBST
autoload -Uz vcs_info

#########
# FIRST #
#########

# %F{magenta}(%f%s%F{magenta})%F{yellow}-%F{magenta}[%F{green}%b%F{yellow}]%f
# becomes (%s)-[%b] if you ignore the colors.
# The %s gets replaced by the vc system (e.g. git)
# And the %b gets replaced by the current branch. 
zstyle ':vcs_info:*' actionformats \
    '%F{magenta}(%f%s%F{magenta})%F{yellow}-%F{magenta}[%F{green}%b%F{3}|%F{1}%a%F{magenta}]%f '
zstyle ':vcs_info:*' formats       \
    '%F{magenta}(%f%s%F{magenta})%F{yellow}-%F{magenta}[%F{green}%b%F{magenta}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{yellow}%r'
zstyle ':vcs_info:*' enable git cvs svn

# or use precmd, see man zshcontrib
vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}

RPROMPT=$'$(vcs_info_wrapper)'

##########
# SECOND #
##########
zstyle ':vcs_info:*' stagedstr 'M' 
zstyle ':vcs_info:*' unstagedstr 'M' 
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' actionformats '%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats \
  '%F{5}[%F{2}%b%F{5}] %F{2}%c%F{3}%u%f'
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked
zstyle ':vcs_info:*' enable git 
+vi-git-untracked() {
  if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
  [[ $(git ls-files --other --directory --exclude-standard | sed q | wc -l | tr -d ' ') == 1 ]] ; then
  hook_com[unstaged]+='%F{1}??%f'
fi
}

precmd () { vcs_info }
PROMPT='%F{5}[%F{2}%n%F{5}] %F{3}%3~ ${vcs_info_msg_0_} %f%# '
