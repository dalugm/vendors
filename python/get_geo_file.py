#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import os
from urllib import request


DAT_URLS = {
    "cdn": [
        "https://cdn.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geoip.dat",
        "https://cdn.jsdelivr.net/gh/Loyalsoldier/v2ray-rules-dat@release/geosite.dat",
    ],
    "github": [
        "https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geoip.dat",
        "https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geosite.dat",
    ],
    "ghproxy": [
        "https://ghproxy.com/https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geoip.dat",
        "https://ghproxy.com/https://github.com/Loyalsoldier/v2ray-rules-dat/releases/latest/download/geosite.dat",
    ],
}

MMDB_URLS = {
    "github": [
        "https://github.com/P3TERX/GeoLite.mmdb/releases/latest/download/GeoLite2-Country.mmdb"
    ],
    "ghproxy": [
        "https://ghproxy.com/https://github.com/P3TERX/GeoLite.mmdb/releases/latest/download/GeoLite2-Country.mmdb"
    ],
}


def download_file(url, path):
    """
    Download a file from the given URL to the specified path.
    """
    try:
        file_name = url.split("/")[-1]
        print(f"\nDownloading from: {url}")
        print(f"Downloading {file_name} to {path}{file_name}")
        request.urlretrieve(
            url, os.path.join(path, file_name), reporthook=print_progress
        )
    except Exception:
        print(Exception)


def print_progress(block_num, block_size, total_size):
    """
    Print the download progress in percentage.
    """
    percent = block_num * block_size * 100 / total_size
    print(f"\r{percent:.2f}% downloaded", end="")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Update geo files.", add_help=True)
    parser.add_argument(
        "-s",
        "--source",
        default="ghproxy",
        choices=["cdn", "github", "ghproxy"],
        help="Data source, default 'ghproxy'.",
    )
    parser.add_argument(
        "-f",
        "--format",
        default="dat",
        choices=["dat", "mmdb"],
        help="File format, default 'dat'.",
    )
    parser.add_argument(
        "-p", "--path", required=True, nargs=1, help="Path to save geo files."
    )

    args = parser.parse_args()

    urls = DAT_URLS[args.source] if args.format == "dat" else MMDB_URLS[args.source]
    for url in urls:
        download_file(url, args.path[0])
