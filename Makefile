CFLAGS = -g -O2 -Wall -Wunused
DBUS_FLAGS = $(shell pkg-config --cflags --libs dbus-1)

bustle-dbus-monitor: bustle-dbus-monitor.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(DBUS_FLAGS)

clean:
	rm -f bustle-dbus-monitor
