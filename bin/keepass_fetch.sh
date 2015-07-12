#!/bin/bash
cd ~/Documents
drive list | grep MyKeePassDatabase.kdbx | awk '{print $1}' | xargs -I{} drive download --id {} --force
