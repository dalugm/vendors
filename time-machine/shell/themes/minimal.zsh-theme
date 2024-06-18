#!/usr/env/bin zsh

local exit_code="%(?,,C:%{%F{red}%}%?%{$reset_color%})"

#
## Sets the prompt statement variables.
#
# Overwrite the default PS1
#   user@hostname ~ %
PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg[yellow]%}%~ "

# Set the default interactive prompt.
#   user@hostname ~ master +!?$
#   $ ...
PS1+="%{%F{white}%}[%*] ${exit_code}"
PS1+='%{%F{magenta}%}$(__git_prompt_info)'
PS1+=$'\n'
PS1+="%f%% "

# Set the continuation interactive prompt.
# > ...
PS2="%{%F{purple}%}> %f"

RPROMPT=''
