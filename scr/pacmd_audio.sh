#!/bin/bash
# Change audio between HDMI | Headphones, and move all input sinks

# Look up these values with `$ pacmd list-sinks | less`
headphones=1
hdmi_bottom_center=2
hdmi_bottom_right=0

# Read input arg and figure out desired sink
set_output_sink() {
    # Default to headphones
    if [ -z $1 ]; then
        echo "Switching to Headphones"
        return $headphones
    elif [ "$1" == "hdmi" ]; then
        echo "Switching to HDMI"
        return $hdmi_bottom_center
    else
        echo "Switching to sink $1"
        return $1
    fi
}

move_sinks() {

    pacmd set-default-sink $1

    # Move the input sinks to the output sink
    pacmd list-sink-inputs | grep index | awk ' {
            print "pacmd move-sink-input " $2 " " outSink "\0"
        }' outSink="$1" | xargs -0 bash -c

    echo ""
}


# $? is the return value of the first function call
set_output_sink $1
move_sinks $?
