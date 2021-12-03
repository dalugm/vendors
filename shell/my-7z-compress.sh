#!/usr/bin/env bash

red='\033[31m'
green='\033[32m'
yellow='\033[33m'
none='\033[0m'

path="$1"
files=$(ls "$path")

OLDIFS=$IFS
IFS=$'\n'

pass2clip()
{
    if [ "$(uname)" = "Darwin" ]; then
        echo "${name%.*}" | pbcopy
    elif [ "$(uname)" = "Linux" ]; then
        echo "${name%.*}" | xsel --clipboard
    else
        echo "Cannot recognize your system..."
    fi
}

for name in $files
do
    printf "\n$yellow *************************************$none \n"
    printf "$green Now you are operating$none  $red %s$none \n\n" "$name"

    name=${name// /\\ }

    printf "$green 1.$none $red Compress $none\n"
    printf "$green 2.$none $red Decompress $none \n"
    printf "$green q.$none $red Exit $none \n"
    printf "$yellow -------------------------------------$none \n"
    printf "$green     Press anykey to continue...     $none \n"
    printf "$yellow -------------------------------------$none \n"

    read -r ch
    case $ch in
        1)
            printf "$yellow -------------------------------------$none \n"
            printf "$green Default password:$none  $red %s$none \n" "${name%.*}"
            printf "$yellow -------------------------------------$none \n"
            if [[ -d $1/${name} ]] || [[ -f $1/${name} ]]; then
                pass2clip
                eval 7z a "$1/${name%.*}".7z "$1/$name" -p
                printf "$yellow -------------------------------------$none \n"
                printf "$green Compress success!$none \n"
                printf "$yellow -------------------------------------$none \n"
                continue
            else
                printf "\n$red ******************************************$none \n"
                printf "\n$red Something wrong happened when compressing.$none \n"
                printf "\n$red ******************************************$none \n"
            fi
            ;;
        2)
            printf "$yellow -------------------------------------$none \n"
            printf "$green Possible password:$none  $red %s$none \n" "${name%.*}"
            printf "$yellow -------------------------------------$none \n"
            if [ -f $1/${name} ]; then
                pass2clip
                7z e "$1/${name%.*}".7z
                continue
            else
                printf "\n$red ******************************************$none \n"
                printf "\n$red Something wrong happened when compressing.$none \n"
                printf "\n$red ******************************************$none \n"
            fi
            ;;
        q)
            exit
            ;;
        *)
            continue
            ;;
    esac
done

IFS=$OLDIFS
