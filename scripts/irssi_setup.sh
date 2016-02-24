#!/bin/bash

# Put the following in /etc/apt/sources.list.d/jgeboski.list
# deb http://download.opensuse.org/repositories/home:/jgeboski/<version> ./

wget -O- https://jgeboski.github.io/obs.key | sudo apt-key add -

sudo apt-get update
sudo apt-get install -y bitlbee-facebook
