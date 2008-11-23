bustle-dbus-monitor: bustle-dbus-monitor.c
	gcc -g `pkg-config --cflags --libs dbus-1 glib-2.0` \
	-Wall -Wunused \
	-o bustle-dbus-monitor bustle-dbus-monitor.c

clean:
	rm -f bustle-dbus-monitor
