#!/bin/sh
exec /usr/bin/firefox--enable-greasemonkey --enable-user-scripts --enable-extensions --user-data-dir=~/.mozilla/$DISPLAY "$@"
