#!/bin/bash
if [ "$#" -eq 0 ]; then
    echo "Need a command"
    exit 1
fi

sleep_duration=5
if [ "$#" -eq 2 ]; then
    sleep_duration=$2
fi

shopt -s expand_aliases

echo -e '\nRunning command "'$1'" every "'$2'" seconds\n'
while [ 1 ]; do
    eval "$1"
    sleep $sleep_duration
done
