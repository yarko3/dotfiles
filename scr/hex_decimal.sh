#!/bin/bash
if [ -z $1 ]; then
    echo "Specify a number"
    exit 1;
fi

echo "Hex --> Decimal"
echo -e "Hex value:\t$1"
echo -e "Dec value:\t$((16#$1))"

# Check if the input could be interpreted as a decimal
if ! [[ $1 =~ [A-Fa-f] ]]; then
    echo ""
    echo "Decimal --> Hex"
    printf 'Hex value:\t%x\n' $1
    echo -e "Dec value:\t$1"
fi
