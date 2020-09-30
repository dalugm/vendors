#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
usage: txtmerge.py <input_dir> <output_file_name>
'''

import os
import sys

assert len(sys.argv) == 3, "usage:\ntxtmerge.py <input_dir> <output_file_name>"

INPUT_DIR = str(sys.argv[1])
OUTPUT_FILE = open(str(sys.argv[2]), 'w', encoding='utf-8')

ENCODE = input('''输入要合并的文件的文本编码（数字或直接输入编码，默认编码为 utf-8）：
1. gb18030
2. utf-8
''')

if ENCODE.isdigit():
    if ENCODE == '1':
        ENCODE = 'gb18030'
    else:
        ENCODE = 'utf-8'
else:
    pass

for file in os.listdir(INPUT_DIR):

    try:
        file = open(os.path.join(INPUT_DIR, file), 'r',
                    encoding=ENCODE)
        text = file.read()

        # make sure there's a line at the end of file
        # (why wouldn't there be one? WINDOWS!
        if text[-1] != u'\n':
            text += u'\n\n'

        OUTPUT_FILE.write(text)
        file.close()

    except IOError:
        print("WARNING: missing location %s" % file)

OUTPUT_FILE.close()
