.PHONY: all
all: init;

.PHONY: install
install:
	./install.sh

.PHONY: init
init:
	./init.sh

.PHONY: distclean
distclean: installer.sh
	rm installer.sh
