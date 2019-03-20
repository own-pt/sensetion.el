#!/bin/bash

sbcl --dynamic-space-size 2Gb --noinform --noprint --eval '(progn (ql:quickload :glosstag) (in-package :glosstag))' --eval '(xml->plist (nth 1 sb-ext:*posix-argv*) (nth 2 sb-ext:*posix-argv*))' --quit $1 $2

