#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import os
import requests

"""
python3 aria2-trackers.py
"""

red = "\033[31m"
green = "\033[32m"
yellow = "\033[33m"
none = "\033[0m"


def get_html(url):
    """
    get html text
    """
    try:
        r = requests.get(url, timeout=30)
        r.raise_for_status()
        r.encoding = "utf-8"
        return r.text
    except:
        print("{0}connection timed out{1}".format(red, none))
        return ""


def change_trackers(text):
    """
    add trackers_website to ~/.aria2/aria2.conf
    """
    try:
        s = text.split("\n")
        string = "bt-tracker="
        string_pattern = "bt-tracker=.*"

        home = os.environ["HOME"]

        file = open(home + "/.aria2/aria2.conf", "r")
        alllines = file.readlines()
        file.close()

        file = open(home + "/.aria2/aria2.conf", "w")

        while "" in s:
            s.remove("")
        string += ",".join(s)

        for line in alllines:
            line_replace = re.sub(string_pattern, string, line)
            file.write(line_replace)

        file.close()
        print("{0}----------{1}".format(yellow, none))
        print("{0}aria2 trackers updated.{1}".format(green, none))
        print("{0}----------{1}".format(yellow, none))
    except:
        print("{0}----------{1}".format(yellow, none))
        print("{0}something wrong happened...{1}".format(red, green))
        print("{0}----------{1}".format(yellow, none))


URL = "https://ngosang.github.io/trackerslist/trackers_best_ip.txt"
TEXT = get_html(URL)
change_trackers(TEXT)
