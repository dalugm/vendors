#!/usr/bin/env sh
##################################################
# name     : tmuxen， tmux environment made easy
# author   : Xu Xiaodong xxdlhy@gmail.com
# license  : GPL
# created  : 2012 Jul 01
# modified : May 02, 2020
##################################################

cmd=$(which tmux) # tmux path
session=daily   # session name

if [ -z $cmd ]; then
  echo "Tmux not installed."
  exit 1
fi

$cmd has -t $session

if [ $? != 0 ]; then
  $cmd new -d -n daily -s $session "daily"
  $cmd splitw -v -p 20 -t $session "edit"
  $cmd neww -n edit -t $session "edit"
  $cmd neww -n program -t $session "program"
  $cmd neww -n update -t $session "update"
  $cmd neww -n aria2 -t $session "aria2"
  $cmd splitw -h -p 50 -t $session "zsh"
  $cmd selectw -t $session:5
fi

$cmd att -t $session

exit 0
