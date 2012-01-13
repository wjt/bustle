/*
 * pcap.h - header for blah blah
 * Copyright ©2011 Collabora Ltd.
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

#ifndef BUSTLE_PCAP_H
#define BUSTLE_PCAP_H

#include <glib-object.h>
#include <gio/gio.h>

typedef struct _BustlePcap BustlePcap;
typedef struct _BustlePcapClass BustlePcapClass;
typedef struct _BustlePcapPrivate BustlePcapPrivate;

struct _BustlePcapClass {
    GObjectClass parent_class;
};

struct _BustlePcap {
    GObject parent;

    BustlePcapPrivate *priv;
};

GType bustle_pcap_get_type (void);

BustlePcap *bustle_pcap_new (
    GBusType bus_type,
    const gchar *filename,
    gboolean verbose,
    GError **error);
void bustle_pcap_stop (
    BustlePcap *self);

/* TYPE MACROS */
#define BUSTLE_TYPE_PCAP \
  (bustle_pcap_get_type ())
#define BUSTLE_PCAP(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST((obj), BUSTLE_TYPE_PCAP, BustlePcap))
#define BUSTLE_PCAP_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_CAST((klass), BUSTLE_TYPE_PCAP,\
                           BustlePcapClass))
#define BUSTLE_IS_PCAP(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE((obj), BUSTLE_TYPE_PCAP))
#define BUSTLE_IS_PCAP_CLASS(klass) \
  (G_TYPE_CHECK_CLASS_TYPE((klass), BUSTLE_TYPE_PCAP))
#define BUSTLE_PCAP_GET_CLASS(obj) \
  (G_TYPE_INSTANCE_GET_CLASS ((obj), BUSTLE_TYPE_PCAP, \
                              BustlePcapClass))

#endif /* BUSTLE_PCAP_H */
