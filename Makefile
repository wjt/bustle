CFLAGS = -g -O2 -Wall -Wunused
DBUS_FLAGS = $(shell pkg-config --cflags --libs dbus-1)
GIO_FLAGS := $(shell pkg-config --cflags --libs glib-2.0 gio-2.0 gio-unix-2.0)
PCAP_FLAGS := $(shell pcap-config --cflags pcap-config --libs)
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

BINARIES = \
	dist/build/bustle-pcap \
	$(NULL)

all: $(BINARIES)

BUSTLE_PCAP_SOURCES = c-sources/pcap-monitor.c c-sources/bustle-pcap.c
BUSTLE_PCAP_HEADERS = c-sources/pcap-monitor.h

dist/build/bustle-pcap: $(BUSTLE_PCAP_SOURCES) $(BUSTLE_PCAP_HEADERS)
	@mkdir -p dist/build
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $(BUSTLE_PCAP_SOURCES) \
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
TARBALL_PARENT_DIR := dist
TARBALL_DIR := $(shell git describe --tags)-$(shell gcc -dumpmachine | perl -pe 's/-.*//')
TARBALL_FULL_DIR := $(TARBALL_PARENT_DIR)/$(TARBALL_DIR)
TARBALL := $(TARBALL_DIR).tar.bz2
maintainer-binary-tarball: all
	mkdir -p $(TARBALL_FULL_DIR)
	cabal-dev configure --prefix=$(TOP)/$(TARBALL_FULL_DIR) \
		--datadir=$(TOP)/$(TARBALL_FULL_DIR) --datasubdir=.
	cabal-dev build
	cabal-dev copy
	cp bustle.sh README $(TARBALL_FULL_DIR)
	perl -pi -e 's{^    bustle-pcap}{    ./bustle-pcap};' \
		-e  's{^    bustle}     {    ./bustle.sh};' \
		$(TARBALL_FULL_DIR)/README
	cp $(BINARIES) $(TARBALL_FULL_DIR)
	mkdir -p $(TARBALL_FULL_DIR)/lib
	cp LICENSE.bundled-libraries $(TARBALL_FULL_DIR)/lib
	cp $(shell ./ldd-me-up.sh $(TARBALL_FULL_DIR)/bin/bustle) $(TARBALL_FULL_DIR)/lib
	cd $(TARBALL_PARENT_DIR) && tar cjf $(TARBALL) $(TARBALL_DIR)
