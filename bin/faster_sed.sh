#! /bin/bash

# sed inline files faster by first grepping for the replacement string
search=$1
replace=$2
target=$(readlink -f "$3")

replace_in_file()
{
    echo "replacing $search with $replace in $target"
    sed -i "s/$search/$replace/g" "$target"
}

replace_in_dir()
{
    if ! compgen -G "$target/*" > /dev/null; then
        echo "no files found in dir=$target"
        exit
    fi

    if ! grep_results=$(grep -l "$search" "$target"/* 2> /dev/null); then
        echo "search string=$search not found in dir=$target"
        exit
    fi
    printf "replacing %s with %s in: \n%s\n" "$search" "$replace" "$grep_results"
    echo "$grep_results" | xargs sed -i "s/$search/$replace/g"
}

if [ -f "$target" ]; then
    replace_in_file
elif [ -d "$target" ]; then
    replace_in_dir
fi
