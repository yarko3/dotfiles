#!/bin/bash
echo "Running cppcheck and displaying errors . . ."
cppcheck . | grep "(error)"
