CC=bootstrap/pratt.py
LISPFILES := $(patsubst %.mule,%.lisp,$(wildcard *.mule))

VPATH = src

all:	build/lib/builtins.lisp
build/lib/builtins.lisp:    src/lib/builtins.mule
	mkdir -p $(dir $@)
	bootstrap/pratt.py $< $@

%.lisp: %.mule
	bootstrap/pratt.py -o $@ $<

clean:
	print $(LISPFILES)
	rm -rf build


# CC=bootstrap/pratt.py
# LISPFILES := $(patsubst %.mule,%.lisp,$(wildcard *.mule))

# VPATH = src

# all:	src
# src:   $(LISPFILES)
# 	bootstrap/pratt.py $(LISPFILES)

# %.lisp: %.mule
# 	bootstrap/pratt.py -o $@ $<

# clean:
# 	print $(LISPFILES)
# 	rm -rf build
