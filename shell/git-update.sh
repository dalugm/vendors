#!/usr/bin/env bash

red='\033[31m'
green='\033[32m'
yellow='\033[33m'
none='\033[0m'

# store the current dir
CUR_DIR=$(pwd)

# Let the person running the script know what's going on.
printf "\n$red[Pulling in latest changes for all repositories...]$none\n"

# Find all git repositories and update it to the latest revision
for i in $(find . -maxdepth 2 | cut -c 3-); do
    printf "\n$green--- ["${i%/*}"] ---$none\n";

    # We have to go to the repository parent directory to call the pull command
    cd "${i%/*}";

    # finally pull
    git pull --rebase;

    # lets get back to the CUR_DIR
    cd $CUR_DIR
done

printf "\n$green[Update complete!]$none\n"
