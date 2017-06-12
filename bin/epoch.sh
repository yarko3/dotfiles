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

main()
{
    epoch_seconds=$(convert_to_epoch_secs "$1")
    date -r "$epoch_seconds"
}

if [ $# -eq 1 ]; then
    main "$1"
else
    usage
fi
