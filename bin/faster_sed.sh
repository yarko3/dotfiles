#! /bin/bash

# sed inline files faster by first grepping for the replacement string
search=$1
replace=$2
target=$3

if [ -f "$target" ]; then
    sed -i "s/$search/$replace/g" "$target"
elif [ -d "$target" ]; then
    if ! compgen -G "$target/*" > /dev/null; then
        echo "no files found in dir=$target"
        exit
    fi

    if ! grep_results=$(grep -l "$search" "$target"/* 2> /dev/null); then
        echo "search string=$search not found in dir=$target"
        exit
    fi
    echo "replacing $search with $replace in $grep_results"
    echo "$grep_results" | xargs sed -i "s/$search/$replace/g"
fi
