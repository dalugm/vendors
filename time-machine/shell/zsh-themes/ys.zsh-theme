# Clean, simple, compatible and meaningful.
# Tested on Linux, Unix and Windows under ANSI colors.
# It is recommended to use with a dark background.
# Colors: black, red, green, yellow, *blue, magenta, cyan, and white.
#
# Mar 2013 Yad Smood
# Modified on 20 September 2020

exit_code="%(?,,C:%{$fg[red]%}%?%{$reset_color%})"

# Prompt format:
#
# PRIVILEGES USER @ MACHINE in DIRECTORY [TIME] BRANCH STATE C:LAST_EXIT_CODE
# $ COMMAND
#
# For example:
#
# % ys @ ys-mbp in ~ [21:47:42] master ! C:0
# $
PROMPT="
%{%B%F{blue}%}#%f%b \
%(#,%{$bg[yellow]%}%{$fg[black]%}%n%{$reset_color%},%{$fg[cyan]%}%n) \
%{%F{white}%}@ \
%{%F{green}%}%m \
%{%F{white}%}in \
%{%B%F{yellow}%}%~%f%b \
%{$fg[white]%}[%*] $exit_code "

PROMPT+='%{%F{magenta}%}$(__git_prompt_info)%f'

PROMPT+=$'\n'
PROMPT+="%{%B%F{red}%}$ %b%f"

RPROMPT=''
