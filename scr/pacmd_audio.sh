#!/bin/bash
# Change audio between HDMI | Headphones, and move all input sinks

# Look up these values with `$ pacmd list-sinks | less`
headphones_sink=1
hdmi_sink=0

# Default to headphones
if [ "$1" == "hdmi" ]; then
    echo "Switching to HDMI"
    outputSink=$hdmi_sink
else
    echo "Switching to Headphones"
    outputSink=$headphones_sink
fi

pacmd set-default-sink $outputSink

# Move the input sinks to the output sink
pacmd list-sink-inputs | grep index | awk ' {
        print "pacmd move-sink-input " $2 " " outSink "\0"
    }' outSink="$outputSink" | xargs -0 bash -c

