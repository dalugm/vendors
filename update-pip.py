#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
update pip packages by cli
"""
import pip
from pip._internal.utils.misc import get_installed_distributions
from subprocess import call

for dist in get_installed_distributions():
    print("%s" %(dist.project_name))
print "----------"

for dist in get_installed_distributions():
    print "updating:%s" %(dist.project_name), "\t"
    call("pip install --user --upgrade " + dist.project_name, shell=True)
