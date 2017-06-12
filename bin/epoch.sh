#! /bin/bash

usage()
{
    echo "$0 usage: <epoch_seconds|epoch_millis>"
    exit 1
}

convert_to_epoch_secs()
{
    epoch=$1
    num_chars_in_epoch_secs=10
    if [ ${#epoch} -gt $num_chars_in_epoch_secs ]; then
        echo $((epoch / 1000))
    else
        echo "$epoch"
    fi
}

print_epoch_secs()
{
    if date --version 2>/dev/null | grep "GNU coreutils" > /dev/null 2>&1; then
        date -d @"$1"
    else
        date -r "$1"
    fi
}

main()
{
    epoch_seconds=$(convert_to_epoch_secs "$1")
    print_epoch_secs "$epoch_seconds"
}

if [ $# -eq 1 ]; then
    main "$1"
else
    usage
fi
