#!/bin/bash

clone_repos() {
    cd ~/
    git clone http://llvm.org/git/llvm.git

    cd ~/llvm/tools
    git clone http://llvm.org/git/clang.git

    cd ~/llvm/projects
    git clone http://llvm.org/git/compiler-rt.git
    git clone http://llvm.org/git/test-suite.git

    cd ~/llvm/tools/clang/tools
    git clone http://llvm.org/git/clang-tools-extra.git
}

build() {
    cd ~/
    mkdir -p build && cd build
    ../llvm/configure
    make
    #sudo make install # hmm?
}
