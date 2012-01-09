CFLAGS = -g -O2 -Wall -Wunused
DBUS_FLAGS = $(shell pkg-config --cflags --libs dbus-1)
GIO_FLAGS := $(shell pkg-config --cflags --libs glib-2.0 gio-2.0 gio-unix-2.0)
PCAP_FLAGS := $(shell pcap-config --cflags pcap-config --libs)
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

BINARIES = \
	dist/build/bustle-dbus-monitor \
	dist/build/bustle-pcap \
	$(NULL)

all: $(BINARIES)

dist/build/bustle-dbus-monitor: c-sources/bustle-dbus-monitor.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(DBUS_FLAGS)

dist/build/bustle-pcap: c-sources/bustle-pcap.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< \
	$(GIO_FLAGS) $(PCAP_FLAGS)

install: all
	mkdir -p $(BINDIR)
	cp $(BINARIES) $(BINDIR)

uninstall:
	rm -f $(notdir $(BINARIES))

clean:
	rm -f $(BINARIES)
	if test -d $(TARBALL_DIR); then rm -r $(TARBALL_DIR); fi
	rm -f $(TARBALL)

# Binary tarball stuff. Please ignore this unless you're making a release.
TOP := $(shell pwd)
TARBALL_DIR := dist/$(shell git describe --tags)-$(shell uname -m)
TARBALL := $(TARBALL_DIR).tar.bz2
maintainer-binary-tarball: all
	mkdir -p $(TARBALL_DIR)
	cabal-dev configure --prefix=$(TOP)/$(TARBALL_DIR) \
		--datadir=$(TOP)/$(TARBALL_DIR) --datasubdir=.
	cabal-dev build
	cabal-dev copy
	cp bustle.sh README $(TARBALL_DIR)
	perl -pi -e 's{^    bustle-pcap}{    ./bustle-pcap};' \
		-e  's{^    bustle}     {    ./bustle.sh};' \
		$(TARBALL_DIR)/README
	cp $(BINARIES) $(TARBALL_DIR)
	tar cjf $(TARBALL) $(TARBALL_DIR)
