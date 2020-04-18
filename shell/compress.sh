#!/usr/bin/env sh

path="$1"
files=$(ls "$path")

pass2clip()
{
    if [ "$(uname)" = "Darwin" ]; then
        echo "${name%.*}" | pbcopy
    elif [ "$(uname)" = "arch" ]; then
        echo "${name%.*}" | pbcopy
    else
        echo "This file can't recognize your system..."
    fi
}

for name in $files
do
    printf "\n\033[33m*************************************\033[0m"
    printf "\n\033[32mNow you are operating\033[0m \033[31m%s\033[0m\n\n" "$name"

    echo "\033[32m1. \033[0m\033[31mZip folder/file with password.\033[0m"
    echo "\033[32m2. \033[0m\033[31mDecrypted zip file.\033[0m"
    echo "\033[32m0. \033[0m\033[31mContinue.\033[0m"
    printf "\033[33m*************************************\033[0m\n"

    read -r ch
    case $ch in
        1)
            printf "\033[32mDefault pasword:\033[0m \033[31m%s\033[0m\n" "${name%.*}"
            printf "\033[33m-------------------------------------\033[0m\n"
            if [ -d "$name" ];
            then
                echo "\033[32mThis is a folder.\033[0m"
                pass2clip
                zip -re "$1/${name%.*}".zip "$name"
                echo "\033[32mCompress success!\033[0m"
                continue
            elif [ -f "$name" ];
            then
                echo "\033[32mThis is a file.\033[0m"
                pass2clip
                zip -e "$1/${name%.*}".zip "$name"
                echo "\033[32mCompress success!\033[0m"
                continue
            else
                printf "\n\033[31m*************************************\033[0m\n"
                echo "\033[31mSomething wrong happened.\033[0m"
                printf "\033[31m*************************************\033[0m\n"
            fi
            ;;
        2)
            printf "\033[32mPossible password:\033[0m \033[31m%s\033[0m\n" "${name%.*}"
            if [ -f "$name" ]
            then
                pass2clip
                unzip "$name"
                continue
            else
                printf "\033[31m*************************************\033[0m\n"
                echo "\033[31mSomething wrong happened.\033[0m"
                printf "\033[31m*************************************\033[0m\n"
            fi
            ;;
        0)
            continue
            ;;
    esac
done
