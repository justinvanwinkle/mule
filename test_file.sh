#!/usr/bin/env bash
python parse_file.py $@ | sbcl --script pretty-print.lisp
