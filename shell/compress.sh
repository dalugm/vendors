#!/usr/bin/env sh

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
        echo "This file can't recognize your system..."
    fi
}

for name in $files
do
    printf "\n\033[33m*************************************\033[0m"
    printf "\n\033[32mNow you are operating\033[0m \033[31m%s\033[0m\n\n" "$name"

    name=${name// /\\ }

    printf "\033[32m1. \033[0m\033[31mZip folder/file with password\033[0m\n"
    printf "\033[32m2. \033[0m\033[31mDecrypted zip file\033[0m\n"
    printf "\033[32mq. \033[0m\033[31mExit\033[0m\n"
    printf "\033[33m-------------------------------------\033[0m\n"
    printf "\033[32m    Press anykey to continue...     \033[0m\n"
    printf "\033[33m*************************************\033[0m\n"

    read -r ch
    case $ch in
        1)
            printf "\033[33m-------------------------------------\033[0m\n"
            printf "\033[32mDefault pasword:\033[0m \033[31m%s\033[0m\n" "${name%.*}"
            printf "\033[33m-------------------------------------\033[0m\n"
            if [ -d $1/${name} ]; then
                pass2clip
                eval zip -re "$1/${name%.*}".zip "$1/$name"
                printf "\033[32mCompress success!\033[0m\n"
                continue
            elif [ -f $1/${name} ]; then
                pass2clip
                eval zip -e "$1/${name%.*}".zip "$1/$name"
                printf "\033[32mCompress success!\033[0m\n"
                continue
            else
                printf "\n\033[31m******************************************\033[0m\n"
                printf "\033[31mSomething wrong happened when compressing.\033[0m\n"
                printf "\033[31m******************************************\033[0m\n"
            fi
            ;;
        2)
            printf "\n\033[33m-------------------------------------\033[0m\n"
            printf "\033[32mPossible password:\033[0m \033[31m%s\033[0m\n" "${name%.*}"
            printf "\033[33m-------------------------------------\033[0m\n"
            if [ -f $1/${name} ]; then
                pass2clip
                unzip "$1/${name%.*}".zip
                continue
            else
                printf "\n\033[31m*****************************************\033[0m\n"
                printf "\033[31mSomething wrong happened when decrypting.\033[0m\n"
                printf "\033[31m*****************************************\033[0m\n"
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
