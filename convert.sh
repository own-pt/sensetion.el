#!/bin/bash

SHOME=/Users/ar/work/sensetion.el

sbcl --dynamic-space-size 10Gb --noinform --noprint --load $SHOME/convert.lisp --eval '(time (main (nth 1 sb-ext:*posix-argv*) (nth 3 sb-ext:*posix-argv*) (nth 2 sb-ext:*posix-argv*)))' --quit $1 $2 $3
