#!/bin/bash
original_mk=scrpxbsvc.mk
gtest_mk=scrpxbsvc_gtest.mk
gtest_dir=tests
gtest_main_obj="scrpxbsvc_gtest.m.t.o"

files=src/*

create_gtest_mk()
{
    if ! [ -f $original_mk ]; then
        echo "Could not find makefile $original_mk"
        exit 1
    fi

    #cp $original_mk $gtest_mk

    cat $gtest_mk | awk '
    {
        if($0 ~ /TASK/ ) {
            print "TASK=" testDir "/" testMain
        }
    }' testDir="$gtest_dir" testMain="$gtest_main_obj"
}

add_test_files()
{
    for file in $files
    do
        if [[ $file =~ "\.t\.cpp" ]]; then
            echo $file
        fi
    done
}


create_gtest_mk
