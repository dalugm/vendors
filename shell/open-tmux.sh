# !/usr/bin/env bash
# Filename: open-tmux.sh
# Author: Xu Xiaodong <xxdlhy@gmail.com>
# Maintainer: dalu <mou.tong@qq.com>
# Created: 2012 Jul 01
# Last Upated: 2020-07-28 12:27
#          By: dalu <mou.tong@qq.com>
# Keywords: tmux
# Version: 0.1
# Changelog:
#     0.1 - initial version
#     0.2 - modified
# Commentary:

# open tmux quickly

# Code:

cmd=$(which tmux)      # tmux path
# session=$(hostname -s) # session name

if [ -z $cmd ]; then
    echo "Tmux not installed."
    exit 1
fi

$cmd has -t $session 2> /dev/null

if [ $? -ne 0 ]; then
    $cmd new-session -d -n daily
    $cmd new-window  -d -n edit
    $cmd new-window  -d -n program
    $cmd new-window  -d -n emacs
    $cmd new-window  -d -n update
    $cmd new-window  -d -n aria2
fi

$cmd attach-session

exit 0
