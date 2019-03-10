#!/bin/bash

SHOME=/Users/ar/work/sensetion.el

sbcl --dynamic-space-size 3Gb --noinform --noprint --load $SHOME/convert.lisp --eval '(identation (nth 1 sb-ext:*posix-argv*) (nth 2 sb-ext:*posix-argv*))' --quit $1 $2

