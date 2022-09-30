# Introduction

## Aria2 auto update

Put [script](trackers-list-aria2.sh) under `/koolshare/aria2` and chmod file to
0755. Then put [this](cru.sh) under `/jffs/scripts` to update automatically.

## xterm-24bit.terminfo

Add a new TERM type to send 24-bit colors to the terminal.

`/usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo`

Then we can use true color (24-bit) in terminal.

``` shell
$ TERM=xterm-24bit
```

NOTE: The princinple is listed below

```
- Local (OS)
    |
    + Terminal (support 24bit color)
        |
        + Shell --------------.
            |                 |
            + Remote          | set same terminfo
                |             |
                + Shell ------'
                    |
                    + Tmux
                        |
                        + Software
```

EXAMPLE:

When using Emacs, you can run this to enable direct color mode in terminal after
Emacs 26.1 ([related commit]):

`$ TERM=xterm-24bit emacs -nw`

Now you can set your $TERM variable to xterm-24bit:

``` sh
export TERM=xterm-24bit
```

SSH might break if your remote machine does not have the same setup, you can work around this with the following alias.

``` sh
alias ssh="TERM=xterm-256color ssh"
```

If you use tmux, you need to make sure you installed a version with truecolor support, then add the following lines to your .tmux.conf:

``` sh
set -g default-terminal "xterm-24bit"
set -g terminal-overrides ',xterm-24bit:Tc'
```

Also, make sure you have utf8 language variables if you want all characters to display correctly. For example:

``` sh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
```

## Delete files

均支持正则表达

- 第一种做法

```sh
find . -depth -name "<name>" -print0 | xargs -0 rm
find . -name '<name>' -type f -delete
```

- 第二种做法

```sh
find ./ -regex "<name>" -print -exec rm -rf {};
```

其中`{}`是`find`命令查找出来的所有结果

## Compress files (WIP)

```sh
bash compress.sh <path>
```

目前存在的问题：

- 只支持`bash`，使用`./`（`POSIX sh`）执行时压缩名字中带空格的文件会报错

- 解压时会将压缩文件解压到当前的工作目录下。

- 文件部分语法不符合规范

[related commit]: https://github.com/emacs-mirror/emacs/commit/e463e5762bbe628be3d15da066a90f079a8468b3
