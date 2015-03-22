CFLAGS = -g -O2 -Wall -Wunused
DBUS_FLAGS = $(shell pkg-config --cflags --libs dbus-1)
GIO_FLAGS := $(shell pkg-config --cflags --libs 'glib-2.0 >= 2.26' gio-2.0 gio-unix-2.0)
PCAP_FLAGS := $(shell pcap-config --cflags pcap-config --libs)
DESTDIR =
PREFIX = /usr/local
BINDIR = $(DESTDIR)$(PREFIX)/bin
DATADIR = $(DESTDIR)$(PREFIX)/share
MAN1DIR = $(DATADIR)/man/man1

BINARIES = \
	dist/build/bustle-pcap \
	$(NULL)

MANPAGE = bustle-pcap.1
DESKTOP_FILE = bustle.desktop
APPDATA_FILE = bustle.appdata.xml
ICON_SIZES = 16x16 22x22 32x32 48x48 256x256
ICONS = \
	data/icons/scalable/bustle.svg \
	data/icons/scalable/bustle-symbolic.svg \
	$(foreach size,$(ICON_SIZES),data/icons/$(size)/bustle.png) \
	$(NULL)

all: $(BINARIES) $(MANPAGE) $(DESKTOP_FILE) $(APPDATA_FILE) $(ICONS)

BUSTLE_PCAP_SOURCES = c-sources/pcap-monitor.c c-sources/bustle-pcap.c
BUSTLE_PCAP_GENERATED_HEADERS = dist/build/autogen/version.h
BUSTLE_PCAP_HEADERS = c-sources/pcap-monitor.h $(BUSTLE_PCAP_GENERATED_HEADERS)

bustle-pcap.1: dist/build/bustle-pcap
	-help2man --output=$@ --no-info --name='Generate D-Bus logs for bustle' $<

bustle.desktop: data/bustle.desktop.in
	LC_ALL=C intltool-merge -d -u po $< $@

bustle.appdata.xml: data/bustle.appdata.xml.in
	LC_ALL=C intltool-merge -x -u po $< $@

dist/build/bustle-pcap: $(BUSTLE_PCAP_SOURCES) $(BUSTLE_PCAP_HEADERS)
	@mkdir -p dist/build
	$(CC) -Idist/build/autogen $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) \
		-o $@ $(BUSTLE_PCAP_SOURCES) \
		$(GIO_FLAGS) $(PCAP_FLAGS)

dist/build/autogen/version.h: bustle.cabal
	@mkdir -p `dirname $@`
	perl -nle 'm/^Version:\s+(.*)$$/ and print qq(#define BUSTLE_VERSION "$$1")' \
		$< > $@

install: all
	mkdir -p $(BINDIR)
	cp $(BINARIES) $(BINDIR)
	-mkdir -p $(MAN1DIR)
	-cp $(MANPAGE) $(MAN1DIR)
	mkdir -p $(DATADIR)/applications
	cp $(DESKTOP_FILE) $(DATADIR)/applications
	mkdir -p $(DATADIR)/appdata
	cp $(APPDATA_FILE) $(DATADIR)/appdata
	$(foreach size,$(ICON_SIZES),mkdir -p $(DATADIR)/icons/hicolor/$(size)/apps; )
	$(foreach size,$(ICON_SIZES),cp data/icons/$(size)/bustle.png $(DATADIR)/icons/hicolor/$(size)/apps; )
	mkdir -p $(DATADIR)/icons/hicolor/scalable/apps
	cp data/icons/scalable/bustle-symbolic.svg $(DATADIR)/icons/hicolor/scalable/apps
	$(MAKE) update-icon-cache

uninstall:
	rm -f $(BINDIR)/$(notdir $(BINARIES))
	rm -f $(MAN1DIR)/$(MANPAGE)
	rm -f $(DATADIR)/applications/$(DESKTOP_FILE)
	rm -f $(DATADIR)/appdata/$(APPDATA_FILE)
	$(foreach size,$(ICON_SIZES),rm -f $(DATADIR)/icons/hicolor/$(size)/apps/bustle.png)
	rm -f $(DATADIR)/icons/hicolor/scalable/apps/bustle-symbolic.svg
	$(MAKE) update-icon-cache

clean:
	rm -f $(BINARIES) $(MANPAGE) $(BUSTLE_PCAP_GENERATED_HEADERS) $(DESKTOP_FILE) $(APPDATA_FILE)
	if test -d ./$(TARBALL_DIR); then rm -r ./$(TARBALL_DIR); fi
	rm -f ./$(TARBALL)

# Icon cache stuff
gtk_update_icon_cache = gtk-update-icon-cache -f -t $(DATADIR)/icons/hicolor

update-icon-cache:
	@-if test -z "$(DESTDIR)"; then \
		echo "Updating GTK+ icon cache."; \
		$(gtk_update_icon_cache); \
	else \
		echo "*** Icon cache not updated.  After (un)install, run this:"; \
		echo "***   $(gtk_update_icon_cache)"; \
	fi

# Binary tarball stuff. Please ignore this unless you're making a release.
TOP := $(shell pwd)
TARBALL_PARENT_DIR := dist
TARBALL_DIR := $(shell git describe --tags)-$(shell gcc -dumpmachine | perl -pe 's/-.*//')
TARBALL_FULL_DIR := $(TARBALL_PARENT_DIR)/$(TARBALL_DIR)
TARBALL := $(TARBALL_DIR).tar.bz2
maintainer-binary-tarball: all
	mkdir -p $(TARBALL_FULL_DIR)
	cabal-dev install --prefix=$(TOP)/$(TARBALL_FULL_DIR) \
		--datadir=$(TOP)/$(TARBALL_FULL_DIR) --datasubdir=.
	cp bustle.sh README.md $(TARBALL_FULL_DIR)
	perl -pi -e 's{^    bustle-pcap}{    ./bustle-pcap};' \
		-e  's{^    bustle}     {    ./bustle.sh};' \
		$(TARBALL_FULL_DIR)/README.md
	cp $(BINARIES) $(MANPAGE) $(DESKTOP_FILE) $(APPDATA_FILE) $(TARBALL_FULL_DIR)
	mkdir -p $(TARBALL_FULL_DIR)/lib
	cp LICENSE.bundled-libraries $(TARBALL_FULL_DIR)/lib
	./ldd-me-up.sh $(TARBALL_FULL_DIR)/bin/bustle \
		| xargs -I XXX cp XXX $(TARBALL_FULL_DIR)/lib
	cd $(TARBALL_PARENT_DIR) && tar cjf $(TARBALL) $(TARBALL_DIR)

maintainer-update-messages-pot:
	find Bustle -name '*.hs' -print0 | xargs -0 hgettext -k __ -o po/messages.pot

maintainer-make-release: bustle.cabal
	cabal test
	cabal sdist
	git tag -s -m 'Bustle '`perl -nle 'm/^Version:\s+(.*)$$/ and print qq($$1)' bustle.cabal` \
		bustle-`perl -nle 'm/^Version:\s+(.*)$$/ and print qq($$1)' bustle.cabal`
	make maintainer-binary-tarball
