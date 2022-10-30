#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import os
import argparse
import requests

"""
python3 update_trackers.py
"""

red = "\033[31m"
green = "\033[32m"
yellow = "\033[33m"
none = "\033[0m"


def get_text(url):
    """
    get html text
    """
    try:
        r = requests.get(url, timeout=30)
        r.raise_for_status()
        r.encoding = "utf-8"
        return r.text
    except Exception:
        print("{0}Timed Out.{1}".format(red, none))
        return ""


def format_to_aria2_trackers(text):
    """
    :return: trackers.
    """
    trackers = text.split("\n")

    while "" in trackers:
        trackers.remove("")

    return ",".join(trackers)


def change_aria2_trackers(text, path):
    """
    change aria2's bt-tracker, default '$HOME/.aria2/aria2.conf'.
    """
    try:
        if path == "home":
            home = os.environ["HOME"]

            file = open(home + "/.aria2/aria2.conf", "r")
            all_lines = file.readlines()
            file.close()

            file = open(home + "/.aria2/aria2.conf", "w")
        else:
            file = open(path, "r")
            all_lines = file.readlines()
            file.close()

            file = open(path, "w")

        string = "bt-tracker=" + text
        string_pattern = "bt-tracker=.*"

        for line in all_lines:
            line_replace = re.sub(string_pattern, string, line)
            file.write(line_replace)

        file.close()

        print("{0}Trackers Updated.{1}".format(green, none))
    except Exception:
        print("{0}Update Failed.{1}".format(red, none))


URL_DICT = {
    "NGOSANG_ALL": "https://ngosang.github.io/trackerslist/trackers_all.txt",
    "NGOSANG_ALL_IP": "https://ngosang.github.io/trackerslist/trackers_all_ip.txt",
    "NGOSANG_BEST": "https://ngosang.github.io/trackerslist/trackers_best.txt",
    "NGOSANG_BEST_IP": "https://ngosang.github.io/trackerslist/trackers_best_ip.txt",
    "XIU2_BEST": "https://trackerslist.com/best.txt",
    "XIU2_ALL": "https://trackerslist.com/all.txt",
    "XIU2_HTTP": "https://trackerslist.com/http.txt",
}


def get_url(str):
    """
    :return: url that str matches
    """
    return URL_DICT[str]


parser = argparse.ArgumentParser(description="Get Latest BT-Trackers.", add_help=True)
parser.add_argument(
    "-s",
    "--source",
    default=["XIU2_BEST"],
    nargs="*",
    help="NGOSANG_{BEST,ALL}[_IP] XIU2_{BEST,ALL,HTTP}",
)
parser.add_argument(
    "-ua",
    "--update-aria2",
    const="home",
    nargs="?",
    help="update aria2 trackers, $HOME/.aria2/aria2.conf default",
)
parser.add_argument(
    "-p", "--print", action="store_true", help="print trackers to console"
)

args = parser.parse_args()

var = ""
for s in args.source:
    url = get_url(s)
    text = get_text(url)
    trackers = format_to_aria2_trackers(text)
    var += trackers + ","
if args.print:
    print(var.replace(",", "\n\n"))
if args.update_aria2:
    # remove the last "," in the string
    trackers = var[:-1]
    change_aria2_trackers(trackers, args.update_aria2)
