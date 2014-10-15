#!/bin/bash

echo "Running clang-format on all cpp and h files in directory"
eval "/opt/bb/bin/clang-format style=file -i *.cpp *.h"

echo "Fixing RCSID comma space"
# SYSUTIL_ or BDES_
eval "sed -i 's/^\([A-Z]*_IDENT_RCSID([A-z_]*,\) /\1/g' *.cpp *.h"
