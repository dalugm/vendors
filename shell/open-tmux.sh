#!/usr/bin/env bash
# Filename: open-tmux.sh
# Author: Xu Xiaodong <xxdlhy@gmail.com>
# Maintainer: dalu <mou.tong@qq.com>
# Created: 2012 Jul 01
# Last Upated: 2020-10-27 14:05
#          By: dalu <mou.tong@qq.com>
# Keywords: tmux
# Version: 0.3
# Changelog:
#     0.1 - initial version
#     0.2 - modified
#     0.3 - setup
# Commentary:

# open tmux quickly

# Code:

cmd=$(which tmux)      # tmux path
session="$1"           # session name

if [ -z "$cmd" ]; then
    echo "Tmux not installed."
    exit 1
fi

if [ -z "$session" ]; then
    session=0
fi

$cmd has-session -t $session 2> /dev/null

if [ "$?" -ne 0 ]; then
    $cmd new-session -s $session -n daily -d
    $cmd send-keys -t $session:1 'cd ~' C-m C-l

    $cmd new-window -t $session:2 -n projs -d
    $cmd send-keys -t $session:2 'cd ~/projs' C-m C-l

    $cmd new-window -t $session:3 -n program -d
    $cmd send-keys -t $session:3 'cd ~/src' C-m C-l
    $cmd split-window -v -t $session:3
    $cmd select-layout -t $session:3 main-horizontal
    # select default tmux layout instead
    # $cmd split-window -v -t $session:3 -p 10
    $cmd send-keys -t $session:3.2 'cd ~' C-m C-l

    $cmd new-window -t $session:4 -d -n emacs
    $cmd send-keys -t $session:4 'cd ~/.emacs.d' C-m C-l

    $cmd new-window -t $session:5 -d -n update
    $cmd send-keys -t $session:5 'cd ~' C-m C-l

    $cmd new-window -t $session:6 -d -n aria2
    $cmd send-keys -t $session:6 'cd ~/.aria2' C-m C-l

    $cmd new-window -t $session:7 -d -n proxy
    $cmd send-keys -t $session:7 'cd ~/tools' C-m C-l

fi

$cmd attach-session -t $session
