#!/usr/bin/env zsh

echo src/test/**/*.py.mule | xargs -t -n1 -P5 python
echo build/test/**/*.lisp | xargs -t -n1 -P5 ./mule
