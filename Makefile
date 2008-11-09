bustle-dbus-monitor: bustle-dbus-monitor.c
	gcc -g `pkg-config --cflags --libs dbus-1` -Wall -Wunused \
	  -o bustle-dbus-monitor bustle-dbus-monitor.c
