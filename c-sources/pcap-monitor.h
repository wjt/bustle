/*
 * pcap-monitor.h - monitors a bus and dumps messages to a pcap file
 * Copyright © 2011–2012 Collabora Ltd.
 * Copyright © 2018      Will Thompson
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

#pragma once

#include <glib-object.h>
#include <gio/gio.h>

#define BUSTLE_TYPE_PCAP_MONITOR bustle_pcap_monitor_get_type ()
G_DECLARE_FINAL_TYPE (BustlePcapMonitor, bustle_pcap_monitor, BUSTLE, PCAP_MONITOR, GObject)

BustlePcapMonitor *bustle_pcap_monitor_new (
    GBusType bus_type,
    const gchar *address,
    const gchar *filename,
    GError **error);
void bustle_pcap_monitor_stop (
    BustlePcapMonitor *self);

extern const char *BUSTLE_MONITOR_NAME_PREFIX;
