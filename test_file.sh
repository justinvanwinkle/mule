#!/usr/bin/env bash
python parse_file.py $1 | sbcl --script pretty-print.lisp
