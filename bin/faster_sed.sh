#! /bin/bash

# sed inline files faster by first grepping for the replacement string
grep_cmd='grep -l'

usage()
{
    echo "$0 usage:"
    echo "-a search string (required)"
    echo "-b replace string (required)"
    echo "-t file/dir (required)"
    echo "-r act recursively if dir provided"
    echo "-c grep with the C language (ASCII only)"
    exit 1
}

replace_in_file()
{
    echo "replacing $search with $replace in $target"
    sed -i "s/$search/$replace/g" "$target"
}

replace_in_dir()
{
    if ! ls -A "$target" > /dev/null; then
        echo "no files found in dir=$target"
        exit
    fi

    if ! grep_results=$(eval "$grep_cmd" "$search" "$target"/* 2> /dev/null); then
        echo "search string=$search not found in dir=$target"
        exit
    fi
    printf "replacing %s with %s in: \n%s\n" "$search" "$replace" "$grep_results"
    echo "$grep_results" | xargs sed -i "s/$search/$replace/g"
}

while getopts "a:b:t:rch" o; do
    case "${o}" in
        a)
            search=${OPTARG}
            ;;
        b)
            replace=${OPTARG}
            ;;
        t)
            target=$(readlink -f "${OPTARG}")
            ;;
        r)
            grep_cmd="$grep_cmd""r"
            echo "will search recursively"
            ;;
        c)
            grep_cmd="LANG=C $grep_cmd"
            echo "will search for ASCII only"
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ ! -n "$search" ] || [ ! -n "$replace" ] || [ ! -n "$target" ]; then
    usage
fi

if [ -f "$target" ]; then
    replace_in_file
elif [ -d "$target" ]; then
    replace_in_dir
else
    usage
fi

