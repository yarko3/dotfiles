#!/bin/bash
cd ~/
mkdir llvm
cd llvm

svn co http://llvm.org/svn/llvm-project/llvm/trunk llvm

cd llvm/tools
svn co http://llvm.org/svn/llvm-project/cfe/trunk clang

cd clang/tools
svn co http://llvm.org/svn/llvm-project/clang-tools-extra/trunk extra

cd ../../../..    #go back to top directory
cd llvm/projects
svn co http://llvm.org/svn/llvm-project/compiler-rt/trunk compiler-rt

cd ../..          #go back to top directory
cd llvm
./configure
make              #this takes a few hours
sudo make install
