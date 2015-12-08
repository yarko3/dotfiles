#!/usr/bin/python
import subprocess

def sh(cmd):
    return subprocess.check_output(cmd, shell=True).strip().split('\n')
