#! /bin/bash

usage()
{
    echo "$0 usage: <epoch_seconds|epoch_millis>"
    exit 1
}

main()
{
    epoch=$1
    num_chars_in_epoch_secs=10
    if [ ${#epoch} -gt $num_chars_in_epoch_secs ]; then
        epoch=$((epoch / 1000))
    fi

    date -r $epoch
}

if [ $# -eq 1 ]; then
    main "$1"
else
    usage
fi
