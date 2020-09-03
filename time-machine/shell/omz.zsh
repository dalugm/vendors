# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.config/zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="random"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# For custom theme use `ln' to add it to candidate
# ln -s "$ZSH_CUSTOM/themes/[custom-theme]/[theme].zsh-theme" "$ZSH/themes/[theme].zsh-theme"

ZSH_THEME_RANDOM_CANDIDATES=(
    evan
    minimal
    robbyrussell
    ys
)

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down  startup.
plugins=(
    autojump
    colored-man-pages
    colorize
    command-not-found
    copydir
    copyfile
    cp
    extract
    fzf
    history
    last-working-dir
    osx
    sudo
    tmux
    web-search
    z
    zsh-autosuggestions
    zsh-completions
    zsh-syntax-highlighting
    zsh_reload
)

source $ZSH/oh-my-zsh.sh

# User configuration

export MANPATH="/usr/local/share/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='nvim'
else
    export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

############################################################
#                      => plugins <=                       #
############################################################

# Config for zsh-completions
autoload -U compinit && compinit
test -e "${HOME}/.iterm2__integration.zsh" && source "${HOME}/.iterm2__integration.zsh"
eval $(thefuck --alias)

# Config for zsh-autosuggestions

# Config for autojump
[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh

# Config for fzf
# Set fzf installation directory path
export FZF_BASE=/usr/local/opt/fzf/install
# Set fzf interactive interface
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse \
    --preview '(highlight -O ansi {} || cat {}) 2> /dev/null | head -500' \
    --bind 'ctrl-n:down,ctrl-p:up,ctrl-j:preview-down,ctrl-k:preview-up'"

# Set default fzf command style
# Use fd instead of ripgrep
# @ https://www.reddit.com/r/linux4noobs/comments/egb644/fzf_newcomer_fd_or_ripgrep/
export FZF_DEFAULT_COMMAND="fd --exclude={.git,.idea,.vscode,.sass-cache,node_modules,build} --hidden --type file"
# Search files cwd
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# Search directory from HOME
export FZF_ALT_C_COMMAND="fd -t d . $HOME"

# Uncomment the following line to disable fuzzy completion
# export DISABLE_FZF_AUTO_COMPLETION="true"

# Uncomment the following line to disable key bindings (CTRL-T, CTRL-R, ALT-C)
# export DISABLE_FZF_KEY_BINDINGS="true"

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

# Personal PATH
export PATH="${HOME}/.emacs.d/bin:$PATH"
export PATH="$HOME/tools/build:$PATH"

# GO
export GOPATH=$HOME/go
export PATH="$GOPATH/bin:$PATH"

# Python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1

if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

# Ruby
export RBENV_ROOT="$HOME/.rbenv"
export PATH="$RBENV_ROOT/bin:$PATH"
export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"

if command -v rbenv 1>/dev/null 2>&1; then
    eval "$(rbenv init -)"
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

############################################################
#                  => personal config <=                   #
############################################################

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
# Example aliases
alias zshconfig="nvim ~/.zshrc"
alias tmuxconfig="nvim ~/.tmux.conf"
alias alaconfig="nvim ~/.alacritty.yml"
alias ytdlconfig="nvim ~/.config/youtube-dl/config"
alias chainizi="python ~/src/misc/zhenghuo/chainizi.py"

alias g='git'
alias vi='vim'
alias nvi='nvim'
alias gvim='/Applications/MacVim.app/Contents/MacOS/MacVim'
alias pc='proxychains4'
alias sicp='mit-scheme'
alias markdown='/Applications/Typora.app/Contents/MacOS/Typora'
alias firefox='/Applications/Firefox.app/Contents/MacOS/firefox'
alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'

alias screen="/usr/local/bin/screen"
alias dscreen="/usr/bin/screen"

# proxy
alias socks5_proxy="export all_proxy=socks5://127.0.0.1:1080; export http_proxy=socks5://127.0.0.1:1080; export https_proxy=socks5://127.0.0.1:1080"
alias http_proxy="export all_proxy=http://127.0.0.1:1087; export http_proxy=http://127.0.0.1:1087; export https_proxy=http://127.0.0.1:1087"
alias clean_proxy="export all_proxy=; export http_proxy=; export https_proxy="

# eXecute Editor
autoload -U   edit-command-line
zle      -N   edit-command-line
bindkey  '^o' edit-command-line

# neovim
if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
    export PS1="〉"
    if [ -x "$(command -v nvr)" ]; then
        alias nvim=nvr
    else
        alias nvim='echo "No nesting!"'
    fi
fi
