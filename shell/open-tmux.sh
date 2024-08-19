#!/usr/bin/env bash

# open tmux quickly

cmd=$(which tmux) # tmux path
session="$1"      # session name

if [ -z "$cmd" ]; then
    echo "Tmux not installed."
    exit 1
fi

if [ -z "$session" ]; then
    session=0
fi

$cmd has-session -t $session 2>/dev/null

if [ "$?" -ne 0 ]; then
    $cmd new-session -s $session -n daily -d

    $cmd new-window -t $session:2 -n projs -d
    $cmd send-keys -t $session:2 'cd ~/projs' C-m C-l

    $cmd new-window -t $session:3 -n program -d
    $cmd split-window -v -t $session:3
    $cmd select-layout -t $session:3 main-horizontal

    # # Select default tmux layout instead.
    # $cmd split-window -v -t $session:3 -p 10

    $cmd new-window -t $session:4 -d -n emacs
    $cmd new-window -t $session:5 -d -n vim

    $cmd new-window -t $session:6 -n downloads -d
    $cmd send-keys -t $session:6 'cd ~/Downloads' C-m C-l

    $cmd new-window -t $session:7 -d -n update -d
    $cmd send-keys -t $session:7 'cd ~/dotfiles' C-m C-l

    $cmd new-window -t $session:8 -d -n tools
    $cmd send-keys -t $session:8 'cd ~/tools' C-m C-l
fi

$cmd attach-session -t $session
