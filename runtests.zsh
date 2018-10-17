#!/usr/bin/env zsh

parallel -t -n1 python ::: src/test/**/*.py.mule
parallel -t -n1 ./mule ::: build/test/**/*.lisp
