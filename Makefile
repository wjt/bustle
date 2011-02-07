bustle-dbus-monitor: bustle-dbus-monitor.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) \
	-g -O2 \
	-Wall -Wunused \
	-o bustle-dbus-monitor bustle-dbus-monitor.c \
	`pkg-config --cflags --libs dbus-1`

clean:
	rm -f bustle-dbus-monitor
