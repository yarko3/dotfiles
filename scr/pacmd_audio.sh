#!/bin/bash
# Change audio between HDMI | Headphones, and move all input sinks

# Look up these values with `$ pacmd list-sinks | less`
outputSink=1
headphones_sink=1

# Bottom center monitor
hdmi_sink=2

# Bottom right monitor
other_hdmi_sink=0


# Read input arg and figure out desired sink
set_output_sink() {
    # Default to headphones
    if [ -z $1 ]; then
        echo "Switching to Headphones"
        outputSink=$headphones_sink
    elif [ "$1" == "hdmi" ]; then
        echo "Switching to HDMI"
        outputSink=$hdmi_sink
    else
        echo "Switching to sink $1"
        outputSink=$1
    fi
}

move_sinks() {

    pacmd set-default-sink $outputSink

    # Move the input sinks to the output sink
    pacmd list-sink-inputs | grep index | awk ' {
            print "pacmd move-sink-input " $2 " " outSink "\0"
        }' outSink="$outputSink" | xargs -0 bash -c

    echo ""
}


set_output_sink $1
move_sinks
