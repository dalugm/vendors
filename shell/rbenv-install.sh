#!/usr/bin/env bash
# Filename: rbenv-install.sh
# Author: dalu <mou.tong@qq.com>
# Maintainer: dalu <mou.tong@qq.com>
# Created: 2020-06-07 00:06
# Last Upated:
#          By:
# Keywords: rbenv, install
# Version: 0.2
# Changelog:
#     0.1 - initial version
# Commentary:

# install ruby version in quick mirrors

# Usage:

# bash rbenv-install.sh <ruby_version>

# Code:

red='\033[31m'
green='\033[32m'
yellow='\033[33m'
none='\033[0m'

if [ -z "$1" ] ; then
    echo -e "$red No parameters detected. $none"
    echo -e "$green usage:$none$yellow ./$0 <ruby_version> $none"
    exit 1;
fi

for v in "$@"; do
    wget https://cache.ruby-china.com/pub/ruby/${v%.*}/ruby-$v.tar.bz2 -P ~/.rbenv/cache/
    rbenv install $v
done
