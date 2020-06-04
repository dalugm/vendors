#!/usr/bin/env bash
# Filename: webm2gif.sh
# Author: dalu <mou.tong@qq.com>
# Maintainer: dalu <mou.tong@qq.com>
# Created: 2020-06-04 11:16
# Last Upated:
#          By:
# Keywords: ffmpeg, video, convert
# Version: 0.1
# Changelog:
#     0.1 - initial version
# Commentary:

# convert cwd multi videos to gifs

# Code:


red='\033[31m'
green='\033[32m'
yellow='\033[33m'
none='\033[0m'

cmd=$(which ffmpeg) # ffmpeg path
suffix="$1"         # video suffix

if [ -z $cmd ]; then
    echo "FFmpeg not installed."
    exit 1
fi

printf "$green Please input video format: $none"

read -r ch
suffix=$ch


for file in $(ls *.$suffix)
do
    $cmd -i $file -vf scale=iw/2:ih/2 -b:v 2400k -r 8 -an "${file%.*}.gif"
done
