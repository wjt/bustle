/*
 * pcap-monitor.h - monitors a bus and dumps messages to a pcap file
 * Copyright ©2011–2012 Collabora Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef BUSTLE_PCAP_MONITOR_H
#define BUSTLE_PCAP_MONITOR_H

#include <glib-object.h>
#include <gio/gio.h>

typedef struct _BustlePcapMonitor BustlePcapMonitor;
typedef struct _BustlePcapMonitorClass BustlePcapMonitorClass;
typedef struct _BustlePcapMonitorPrivate BustlePcapMonitorPrivate;

struct _BustlePcapMonitorClass {
    GObjectClass parent_class;
};

struct _BustlePcapMonitor {
    GObject parent;

    BustlePcapMonitorPrivate *priv;
};

GType bustle_pcap_monitor_get_type (void);

BustlePcapMonitor *bustle_pcap_monitor_new (
    GBusType bus_type,
    const gchar *filename,
    GError **error);
void bustle_pcap_monitor_stop (
    BustlePcapMonitor *self);

/* TYPE MACROS */
#define BUSTLE_TYPE_PCAP_MONITOR \
  (bustle_pcap_monitor_get_type ())
#define BUSTLE_PCAP_MONITOR(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST((obj), BUSTLE_TYPE_PCAP_MONITOR, BustlePcapMonitor))
#define BUSTLE_PCAP_MONITOR_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_CAST((klass), BUSTLE_TYPE_PCAP_MONITOR,\
                           BustlePcapMonitorClass))
#define BUSTLE_IS_PCAP_MONITOR(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE((obj), BUSTLE_TYPE_PCAP_MONITOR))
#define BUSTLE_IS_PCAP_MONITOR_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_TYPE((klass), BUSTLE_TYPE_PCAP_MONITOR))
#define BUSTLE_PCAP_MONITOR_GET_CLASS(obj) \
  (G_TYPE_INSTANCE_GET_CLASS ((obj), BUSTLE_TYPE_PCAP_MONITOR, \
                              BustlePcapMonitorClass))

#endif /* BUSTLE_PCAP_MONITOR_H */
