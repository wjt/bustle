bustle-dbus-monitor: bustle-dbus-monitor.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) \
	-g -O2 `pkg-config --cflags --libs dbus-1` \
	-Wall -Wunused \
	-o bustle-dbus-monitor bustle-dbus-monitor.c

clean:
	rm -f bustle-dbus-monitor
