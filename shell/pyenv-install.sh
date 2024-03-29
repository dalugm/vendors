#!/usr/bin/env bash
# Filename: pyenv-install.sh
# Author: dalu <mou.tong@qq.com>
# Maintainer: dalu <mou.tong@qq.com>
# Created: 2020-05-01 15:57
# Last Upated:
#          By:
# Keywords: pyenv, install
# Version: 0.2
# Changelog:
#     0.1 - Initial version
#     0.2 - Add multi version support
#     0.3 - Replace wget with curl
# Commentary:

# Install python version in quick mirrors

# Usage:

# bash pyenv-install.sh <python_version>

# Code:

red='\033[31m'
green='\033[32m'
yellow='\033[33m'
none='\033[0m'

if [ -z "$1" ] ; then
    echo -e "$red No parameters detected. $none"
    echo -e "$green usage:$none$yellow ./$0 <python_version> $none"
    exit 1;
fi

for v in "$@"; do
    curl "https://repo.huaweicloud.com/python/$v/Python-$v.tar.xz" \
         -o "$HOME/.pyenv/cache/Python-$v.tar.xz"
    pyenv install "$v"
done
