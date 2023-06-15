#!/bin/sh
list=$(curl https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all.txt | awk NF | sed ":a;N;s/\n/,/g;ta")

if  "$(grep -q "bt-tracker" /koolshare/aria2/aria2.conf)" ; then
    sed -i '$a bt-tracker='"${list}" /koolshare/aria2/aria2.conf
    echo add......
else
    sed -i "s@bt-tracker.*@bt-tracker=$list@g" /koolshare/aria2/aria2.conf
    echo update......
fi

sleep 1

sh /koolshare/aria2/aria2_run.sh
