#!/bin/bash

echo "Running clang-format on all cpp and h files in directory"
/opt/bb/bin/clang-format -i *.cpp *.h

echo "Fixing RCSID comma space"
# SYSUTIL_ or BDES_
sed -i 's/^\([A-Z]*_IDENT_RCSID([A-z_]*,\) /\1/g' *.cpp *.h
