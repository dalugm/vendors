#!/usr/bin/env sh

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
        echo "This file can't recognize your system..."
    fi
}

for name in $files
do
    echo "${yellow}*************************************${none}"
    printf "${green}Now you are operating${none} ${red}%s${none}\n\n" "$name"

    name=${name// /\\ }

    echo "${green}1.${none} ${red}Zip folder/file with password${none}"
    echo "${green}2.${none} ${red}Decrypted zip file${none}"
    echo "${green}q.${none} ${red}Exit${none}"
    echo "${yellow}-------------------------------------${none}"
    echo "${green}    Press anykey to continue...     ${none}"
    echo "${yellow}-------------------------------------${none}"

    read -r ch
    case $ch in
        1)
            echo "${yellow}-------------------------------------${none}"
            printf "${green}Default pasword:${none} ${red}%s${none}\n" "${name%.*}"
            echo "${yellow}-------------------------------------${none}"
            if [ -d $1/${name} ]; then
                pass2clip
                eval zip -re "$1/${name%.*}".zip "$1/$name"
                echo "${green}Compress success!${none}"
                echo ""
                continue
            elif [ -f $1/${name} ]; then
                pass2clip
                eval zip -e "$1/${name%.*}".zip "$1/$name"
                echo "${green}Compress success!${none}"
                echo ""
                continue
            else
                echo ""
                echo "${red}******************************************${none}"
                echo "${red}Something wrong happened when compressing.${none}"
                echo "${red}******************************************${none}"
                echo ""
            fi
            ;;
        2)
            echo "${yellow}-------------------------------------${none}"
            printf "${green}Possible password:${none} ${red}%s${none}\n" "${name%.*}"
            echo "${yellow}-------------------------------------${none}"
            if [ -f $1/${name} ]; then
                pass2clip
                unzip "$1/${name%.*}".zip
                continue
            else
                echo ""
                echo "${red}*****************************************${none}"
                echo "${red}Something wrong happened when decrypting.${none}"
                echo "${red}*****************************************${none}"
                echo ""
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
