declare -A ZINIT  # initial Zinit's hash definition, if configuring before loading Zinit, and then:

ZINIT[HOME_DIR]="$HOME/.config/zinit"
ZINIT[BIN_DIR]="${ZINIT[HOME_DIR]}/bin"

# Ask to download zinit if NOT installed.
if [ ! -d "${ZINIT[BIN_DIR]}" ]; then
    printf "Zinit not installed, clone to ${ZINIT[BIN_DIR]}? [y/N]: " 
    read installp
    if [ "$installp" = "y" ]; then
        git clone --depth=1 "https://github.com/zdharma/zinit.git" "${ZINIT[BIN_DIR]}"
    fi
fi

source "${ZINIT[BIN_DIR]}/zinit.zsh"

# load plugins with a single command. 
# For more information see:
# https://zdharma.org/zinit/wiki/For-Syntax/
zinit light-mode for \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node \
    zdharma/history-search-multi-word \
    pick"async.zsh" src"pure.zsh" \
        sindresorhus/pure

# 快速目录跳转
zinit ice lucid wait='1'
zinit light skywind3000/z.lua

# 语法高亮
zinit ice lucid wait='0' atinit='zpcompinit'
zinit light zdharma/fast-syntax-highlighting

# 自动建议
zinit ice lucid wait="0" atload='_zsh_autosuggest_start'
zinit light zsh-users/zsh-autosuggestions

# 补全
zinit ice lucid wait='0'
zinit light zsh-users/zsh-completions

############################################################
#                        => OMZ <=                         #
############################################################

# 加载 OMZ 框架及部分插件
zinit snippet OMZ::lib/completion.zsh
zinit snippet OMZ::lib/history.zsh
zinit snippet OMZ::lib/key-bindings.zsh
zinit snippet OMZ::lib/theme-and-appearance.zsh
zinit snippet OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh
zinit snippet OMZ::plugins/sudo/sudo.plugin.zsh

############################################################
#                      => plugins <=                       #
############################################################

# Config for zsh-completions
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Config for fzf
# Set fzf installation directory path
export FZF_BASE=/usr/local/opt/fzf/install
# Set fzf interactive interface
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse \
    --preview '(highlight -O ansi {} || cat {}) 2> /dev/null | head -500' \
    --bind 'j:down,k:up,ctrl-j:preview-down,ctrl-k:preview-up'"

# Set default omitted dirs
export FZF_DEFAULT_COMMAND="fd --exclude={.git,.idea,.vscode,.sass-cache,node_modules,build} --type f"

############################################################
#                  => personal config <=                   #
############################################################

export TERM=screen-256color

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
# Example aliases
alias zshconfig="nvim ~/.zshrc"
alias tmuxconfig="nvim ~/.tmux.conf.local"
alias ytdlconfig="nvim ~/.config/youtube-dl/config"

alias g='git'
alias vi='vim'
alias nvi='nvim'
alias pc='proxychains4'
alias sicp="mit-scheme"
alias markdown='/Applications/Typora.app/Contents/MacOS/Typora'
alias firefox='/Applications/Firefox.app/Contents/MacOS/firefox'
alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'

alias screen="/usr/local/bin/screen"
alias dscreen="/usr/bin/screen"

# 用 vim 编辑命令行 `C-o'
autoload -U  edit-command-line
zle -N       edit-command-line
bindkey '^o' edit-command-line

############################################################
#                      => export <=                        #
############################################################

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/lib/ruby/gems/2.7.0/bin:$PATH"
export PATH="/usr/local/opt/ruby/bin:$PATH"
export PATH="/usr/local/opt/curl/bin:$PATH"
export PATH="/usr/local/opt/sqlite/bin:$PATH"
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
export PATH="/usr/local/opt/sphinx-doc/bin:$PATH"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export PATH="/usr/local/opt/texinfo/bin:$PATH"
export PATH="/usr/local/opt/gettext/bin:$PATH"
export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"
export PATH="/usr/local/opt/qt/bin:$PATH"
export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"

# GO
export GOPATH=$HOME/go
export PATH="$GOPATH/bin:$PATH"

# Python
export PATH="$HOME/Library/Python/3.7/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# HOMEBREW
# 关闭 homebrew 自动更新
export HOMEBREW_NO_AUTO_UPDATE=true
export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles"

# For compilers to find software
export LDFLAGS="-L/usr/local/lib"
export CPPFLAGS="-I/usr/local/include"

# For pkg-config to find software
export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/opt/imagemagick@6/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/opt/curl/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/opt/imagemagick@6/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/opt/qt/lib/pkgconfig:$PKG_CONFIG_PATH"
