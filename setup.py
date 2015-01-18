#!/usr/bin/env python2.7

from setuptools import setup

setup(
    name='bap',
    version='0.9.1',
    package_dir = {'bap' : 'python'},
    packages = ['bap'],
    install_requires = ['requests']
)
