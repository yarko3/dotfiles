#!/bin/bash

cd ~/mbig
rm -rf ycm_build
mkdir ycm_build
cd ycm_build

# Without clang / c support
#cmake -G "Unix Makefiles" -DPYTHON_LIBRARY=/opt/swt/lib64/libpython2.7.so -DPYTHON_INCLUDE_DIR=/opt/swt/include/python2.7 . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp

# With clang
cmake -G "Unix Makefiles" -DPYTHON_LIBRARY=/opt/swt/lib64/libpython2.7.so -DPYTHON_INCLUDE_DIR=/opt/swt/include/python2.7 -DEXTERNAL_LIBCLANG_PATH=/bb/sys/package/opt/bb/lib64/libclang.so . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp

make ycm_support_libs
cd ..
rm -rf ycm_build
