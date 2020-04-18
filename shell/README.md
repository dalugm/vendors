# Introduction

## Delete files

均支持正则表达

- 第一种做法

```sh
find . -name "<name>" -depth -print0 | xargs -0 rm
find . -name '<name>' -type f -delete
```

- 第二种做法

```sh
find ./ -regex "<name>" -print -exec rm -fr {};
```

其中`{}`是`find`命令查找出来的所有结果

## Compress files (WIP)

```sh
./compress.sh <path>
```
