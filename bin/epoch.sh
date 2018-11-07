#!/usr/bin/env bash

convert_to_epoch_secs()
{
  epoch=$1
  num_chars_in_epoch_secs=10
  # assuming nobody is gonna be using this script past 2286; plz prove me wrong
  if [ ${#epoch} -gt $num_chars_in_epoch_secs ]; then
    echo $((epoch / 1000))
  else
    echo "$epoch"
  fi
}

print_human_readable()
{
  if date --version 2>/dev/null | grep "GNU coreutils" > /dev/null 2>&1; then
    date -d @"$1"
  else
    date -r "$1"
  fi
}

print_epoch_seconds()
{
  epoch_seconds=$(convert_to_epoch_secs "$1")
  print_human_readable "$epoch_seconds"
}

if [ $# -eq 1 ]; then
  print_epoch_seconds "$1"
else
  date +'%s'
fi
