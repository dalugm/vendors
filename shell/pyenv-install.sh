#!/usr/bin/env bash
# Filename: pyenv-install.sh
# Author: dalu <mou.tong@qq.com>
# Maintainer: dalu <mou.tong@qq.com>
# Created: 2020-05-01 15:57
# Last Upated:
#          By:
# Keywords: pyenv, install
# Version: 0.1
# Changelog:
#     0.1 - initial version
# Commentary:

# install python version in qucik mirrors

# Usage:

# bash pyenv-install.sh <python_version>

# Code:

v=${1}
wget https://npm.taobao.org/mirrors/python/$v/Python-$v.tar.xz -P ~/.pyenv/cache/
pyenv install $v 
