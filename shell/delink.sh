for f in $(find $1 -type l); do [ -e $f ] && rm -f $f; done
