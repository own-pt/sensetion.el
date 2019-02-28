#!/bin/bash

GLOSS=$1
INDEX=$2
OUTPUT=$3

sbcl --dynamic-space-size 10Gb --noinform --noprint --load convert.lisp --eval '(time (main (nth 1 sb-ext:*posix-argv*) (nth 3 sb-ext:*posix-argv*) (nth 2 sb-ext:*posix-argv*)))' --quit $GLOSS $INDEX $OUTPUT

