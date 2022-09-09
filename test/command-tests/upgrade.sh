#!/bin/sh

for output in *.output; do
    file=$(basename $output .output)
    expected=$file.expected
    echo mv $output $expected
    mv $output $expected
done
