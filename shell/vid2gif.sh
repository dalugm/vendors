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
#     0.2 - add arguments
# Commentary:

# convert cwd multi videos to gifs

# Code:


cmd=$(which ffmpeg) # ffmpeg path

if [ -z $cmd ]; then
    echo "FFmpeg not installed."
    exit 1
fi

read -p "Input video format: " suffix
read -p "Input frame rate (the bigger the more fluent): " rate


for file in $(ls *.$suffix)
do
    if [ "$1" == "-c" ]; then
        $cmd -i $file -vf scale=iw/2:ih/2 -b:v 2400k -r "$rate" -an "${file%.*}.gif"
    else
        $cmd -i $file -b:v 2400k -r "$rate" -an "${file%.*}.gif"
    fi
done
