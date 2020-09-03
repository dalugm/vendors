# Introduction

## terminfo-xterm-24bits.src

`tic -x -o ~/.terminfo terminfo-xterm-24bits.src`

Then we can use true color (24-bits) in terminal.

``` shell
$ TERM=xterm-24bits
```

NOTE: The princinple is listed below

```
- Local (OS)
    |
    + Terminal (support 24bits color)
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


## Delete files
均支持正则表达

- 第一种做法

```sh
find . -depth -name "<name>" -print0 | xargs -0 rm
find . -name '<name>' -type f -delete
```

- 第二种做法

```sh
find ./ -regex "<name>" -print -exec rm -fr {};
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
