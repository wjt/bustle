CFLAGS = -g -O2 -Wall -Wunused
DBUS_FLAGS = $(shell pkg-config --cflags --libs dbus-1)
GIO_FLAGS := $(shell pkg-config --cflags --libs glib-2.0 gio-2.0 gio-unix-2.0)
PCAP_FLAGS := $(shell pcap-config --cflags pcap-config --libs)
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin

all: bustle-dbus-monitor bustle-pcap

bustle-dbus-monitor: bustle-dbus-monitor.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(DBUS_FLAGS)

bustle-pcap: bustle-pcap.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< \
	$(GIO_FLAGS) $(PCAP_FLAGS)

install: all
	mkdir -p $(BINDIR)
	cp bustle-dbus-monitor bustle-pcap $(BINDIR)

uninstall:
	rm -f $(BINDIR)/bustle-dbus-monitor $(BINDIR)/bustle-pcap

clean:
	rm -f bustle-dbus-monitor bustle-pcap
	if test -d $(TARBALL_DIR); then rm -r $(TARBALL_DIR); fi

# Binary tarball stuff. Please ignore this unless you're making a release.
TOP := $(shell pwd)
TARBALL_DIR := $(shell git describe --tags)-$(shell uname -m)
maintainer-binary-tarball: all
	mkdir -p $(TARBALL_DIR)
	cabal-dev configure --prefix=$(TOP)/$(TARBALL_DIR) \
		--datadir=$(TOP)/$(TARBALL_DIR) --datasubdir=.
	cabal-dev build
	cabal-dev copy
	cp bustle.sh $(TARBALL_DIR)
	cp bustle-dbus-monitor bustle-pcap $(TARBALL_DIR)/bin
	tar cjf $(TARBALL_DIR).tar.bz2 $(TARBALL_DIR)
