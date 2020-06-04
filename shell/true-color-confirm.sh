# Filename: true-color-confirm.sh
# Author: dalu <mou.tong@qq.com>
# Maintainer: dalu <mou.tong@qq.com>
# Created: 2020-05-02 23:26
# Last Upated:
#          By:
# Keywords: keywords
# Version: 0.1
# Changelog:
#     0.1 - initial version
# Commentary:

# verify whether current terminal support 24-bit color

# Code:

awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";
}'
