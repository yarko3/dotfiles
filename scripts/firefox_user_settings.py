#!/usr/bin/python
import os
import sys
home = os.path.expanduser("~")
sys.path.append(home + "/bin")
from sh import *

def link(profDir):
    src = home + "/dotfiles/mozilla/user.js"
    tar = profDir + "/user.js"
    sh("ln -fs " + src + " " + tar)

# Links the user.js file into all Mozilla Profiles
def main():
    os.chdir(home + "/.mozilla/firefox")
    profiles = sh("grep Path= profiles.ini")
    profiles = map(lambda x: x.split("=")[1], profiles)
    map(link, profiles)
    print "Mapped user.js into the following profiles:"
    for p in profiles:
        print "\t" + p

if __name__ == "__main__":
    main()
