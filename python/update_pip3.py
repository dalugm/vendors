#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
update pip3 packages by cli
"""

from subprocess import call
from pip._internal.utils.misc import get_installed_distributions

for dist in get_installed_distributions():
    print(dist.project_name)

for dist in get_installed_distributions():
    print("\n----- updating: [ ", dist.project_name, " ] -----\t\n")
    call("pip3 install --user --upgrade " + dist.project_name, shell=True)
