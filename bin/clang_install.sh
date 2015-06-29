#!/bin/bash

if [[ "$USER" == "bhipple" ]]; then
    BASE_DIR="/bb/bigstorq4/scrpbuild/ben"
else
    BASE_DIR="/home/$USER"
fi

clone_repos() {
    cd "$BASE_DIR"
    git clone http://llvm.org/git/llvm.git

    cd "$BASE_DIR"/llvm/tools
    git clone http://llvm.org/git/clang.git

    cd "$BASE_DIR"/llvm/projects
    git clone http://llvm.org/git/compiler-rt.git
    git clone http://llvm.org/git/test-suite.git

    cd "$BASE_DIR"/llvm/tools/clang/tools
    git clone http://llvm.org/git/clang-tools-extra.git
}

build() {
    cd "$BASE_DIR"
    mkdir -p build && cd build
    ../llvm/configure
    make
    #sudo make install # hmm?
}

clone_repos
build
