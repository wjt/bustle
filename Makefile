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
