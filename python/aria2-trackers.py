#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import os
import requests

'''
python3 aria2-trackers.py
'''

def get_html(url):
    """
    get html text
    """
    try:
        r = requests.get(url, timeout=30)
        r.raise_for_status()
        r.encoding = 'utf-8'
        return r.text
    except:
        print("connection timed out")
        return ""

def change_trackers(text):
    """
    add website to ~/.aria2/aria2.conf
    """
    s = text.split('\n')
    string = "bt-tracker="
    string_pattern = "bt-tracker=.*"

    home = os.environ['HOME']

    file = open(home+'/.aria2/aria2.conf', "r")
    alllines = file.readlines()
    file.close()

    file = open(home+'/.aria2/aria2.conf', "w")

    while '' in s:
        s.remove('')
    string += ",".join(s)

    for line in alllines:
        line_replace = re.sub(string_pattern, string, line)
        file.write(line_replace)

    file.close()

URL = 'https://ngosang.github.io/trackerslist/trackers_best_ip.txt'
TEXT = get_html(URL)
change_trackers(TEXT)
