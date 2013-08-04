CC=bootstrap/pratt.py

SOURCE_DIR = src
BUILD_DIR = build

MULEFILES := $(shell find $(SOURCE_DIR) -name "*.mule")
LISPFILES := $(MULEFILES:$(SOURCE_DIR)%.mule=$(BUILD_DIR)%.lisp)



all:	$(LISPFILES)
$(LISPFILES):  $(MULEFILES)
	mkdir -p $(dir $@)
	@$(CC) $^ $@

clean:
	print $(LISPFILES)
	rm -rf build
