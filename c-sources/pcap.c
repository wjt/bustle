/*
 * pcap.c - blah blah
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

#include "pcap.h"

#include <string.h>
#include <pcap/pcap.h>

/* Not using GString because it holds signed chars but we both receive and
 * provide unsigned chars. C!
 */
typedef struct {
    struct timeval ts;
    gsize size;
    guchar *blob;
} Message;

#define STOP ((Message *) 0x1)

typedef struct {
    pcap_dumper_t *dumper;
    GAsyncQueue *message_queue;
} ThreadData;

struct _BustlePcapPrivate {
    GBusType bus_type;
    GDBusConnection *connection;
    GDBusCapabilityFlags caps;

    guint filter_id;

    gchar *filename;
    pcap_t *p;

    GThread *thread;
    ThreadData td;

    /* FIXME: this does not really belong here. main() should connect to
     * ::message-logged when it provides enough details. */
    gboolean verbose;
};

enum {
    PROP_BUS_TYPE = 1,
    PROP_FILENAME,
    PROP_VERBOSE,
};

enum {
    SIG_MESSAGE_LOGGED,
    N_SIGNALS
};

static guint signals[N_SIGNALS];

static void initable_iface_init (
    gpointer g_class,
    gpointer unused);

G_DEFINE_TYPE_WITH_CODE (BustlePcap, bustle_pcap, G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE (G_TYPE_INITABLE, initable_iface_init);
    )

static void
bustle_pcap_init (BustlePcap *self)
{
  self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self, BUSTLE_TYPE_PCAP,
      BustlePcapPrivate);
  self->priv->bus_type = G_BUS_TYPE_SESSION;
  self->priv->td.message_queue = g_async_queue_new ();
}

static void
bustle_pcap_get_property (
    GObject *object,
    guint property_id,
    GValue *value,
    GParamSpec *pspec)
{
  BustlePcap *self = BUSTLE_PCAP (object);
  BustlePcapPrivate *priv = self->priv;

  switch (property_id)
    {
      case PROP_BUS_TYPE:
        g_value_set_enum (value, priv->bus_type);
        break;
      case PROP_FILENAME:
        g_value_set_string (value, priv->filename);
        break;
      case PROP_VERBOSE:
        g_value_set_boolean (value, priv->verbose);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
bustle_pcap_set_property (
    GObject *object,
    guint property_id,
    const GValue *value,
    GParamSpec *pspec)
{
  BustlePcap *self = BUSTLE_PCAP (object);
  BustlePcapPrivate *priv = self->priv;

  switch (property_id)
    {
      case PROP_BUS_TYPE:
        priv->bus_type = g_value_get_enum (value);
        break;
      case PROP_FILENAME:
        priv->filename = g_value_dup_string (value);
        break;
      case PROP_VERBOSE:
        priv->verbose = g_value_get_boolean (value);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
bustle_pcap_dispose (GObject *object)
{
  BustlePcap *self = BUSTLE_PCAP (object);
  BustlePcapPrivate *priv = self->priv;
  GObjectClass *parent_class = bustle_pcap_parent_class;

  if (parent_class->dispose != NULL)
    parent_class->dispose (object);

  /* Make sure we're all closed up. */
  bustle_pcap_stop (self);

  g_clear_object (&priv->connection);
}

static void
bustle_pcap_finalize (GObject *object)
{
  BustlePcap *self = BUSTLE_PCAP (object);
  BustlePcapPrivate *priv = self->priv;
  GObjectClass *parent_class = bustle_pcap_parent_class;

  if (parent_class->finalize != NULL)
    parent_class->finalize (object);

  g_free (priv->filename);
  priv->filename = NULL;

  g_async_queue_unref (priv->td.message_queue);
  priv->td.message_queue = NULL;
}

static void
bustle_pcap_class_init (BustlePcapClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  GParamSpec *param_spec;

  object_class->get_property = bustle_pcap_get_property;
  object_class->set_property = bustle_pcap_set_property;
  object_class->dispose = bustle_pcap_dispose;
  object_class->finalize = bustle_pcap_finalize;

  g_type_class_add_private (klass, sizeof (BustlePcapPrivate));

#define THRICE(x) x, x, x

  param_spec = g_param_spec_enum (THRICE ("bus-type"),
      G_TYPE_BUS_TYPE, G_BUS_TYPE_SESSION,
      G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (object_class, PROP_BUS_TYPE, param_spec);

  param_spec = g_param_spec_string (THRICE ("filename"), NULL,
      G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (object_class, PROP_FILENAME, param_spec);

  param_spec = g_param_spec_boolean (THRICE ("verbose"), FALSE,
      G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
  g_object_class_install_property (object_class, PROP_VERBOSE, param_spec);

  signals[SIG_MESSAGE_LOGGED] = g_signal_new ("message-logged",
      BUSTLE_TYPE_PCAP, G_SIGNAL_RUN_FIRST,
      0, NULL, NULL,
      NULL, G_TYPE_NONE, 0);
}

static gpointer
log_thread (gpointer data)
{
  ThreadData *td = data;
  Message *message;

  while (STOP != (message = g_async_queue_pop (td->message_queue)))
    {
      struct pcap_pkthdr hdr;
      hdr.ts = message->ts;

      hdr.caplen = message->size;
      hdr.len = message->size;

      /* The cast is necessary because libpcap is weird. */
      pcap_dump ((u_char *) td->dumper, &hdr, message->blob);
      g_free (message->blob);
      g_slice_free (Message, message);
    }

  return NULL;
}

static gboolean
emit_me (gpointer data)
{
  BustlePcap *self = BUSTLE_PCAP (data);

  g_signal_emit (self, signals[SIG_MESSAGE_LOGGED], 0);
  g_object_unref (self);
  return FALSE;
}

GDBusMessage *
filter (
    GDBusConnection *connection,
    GDBusMessage *message,
    gboolean is_incoming,
    gpointer user_data)
{
  BustlePcap *self = BUSTLE_PCAP (user_data);
  const gchar *dest;
  Message m;
  GError *error = NULL;

  gettimeofday (&m.ts, NULL);
  m.blob = g_dbus_message_to_blob (message, &m.size, self->priv->caps, &error);
  if (m.blob == NULL)
    {
      g_critical ("Couldn't marshal message: %s", error->message);
      g_return_val_if_reached (NULL);
    }
  /* An ugly hack to push the signal into the UI thread. */
  g_idle_add (emit_me, g_object_ref (self));
  g_async_queue_push (self->priv->td.message_queue, g_slice_dup (Message, &m));

  dest = g_dbus_message_get_destination (message);

  if (self->priv->verbose)
    g_print ("(%s) %s -> %s: %u %s\n",
        is_incoming ? "incoming" : "outgoing",
        g_dbus_message_get_sender (message),
        dest,
        g_dbus_message_get_message_type (message),
        g_dbus_message_get_member (message));

  if (!is_incoming ||
      g_strcmp0 (dest, g_dbus_connection_get_unique_name (connection)) == 0)
    {
      /* This message is either outgoing or actually for us, as opposed to
       * being eavesdropped. */
      return message;
    }

  /* We have to say we've handled the message, or else GDBus replies to other
   * people's method calls and we all get really confused.
   */
  g_object_unref (message);
  return NULL;
}

static gboolean
match_everything (
    GDBusProxy *bus,
    GError **error)
{
  char *rules[] = {
      "type='signal'",
      "type='method_call'",
      "type='method_return'",
      "type='error'",
      NULL
  };
  char **r;

  for (r = rules; *r != NULL; r++)
    {
      GVariant *ret = g_dbus_proxy_call_sync (
          bus,
          "AddMatch",
          g_variant_new ("(s)", *r),
          G_DBUS_CALL_FLAGS_NONE,
          -1,
          NULL,
          error);

      if (ret == NULL)
        {
          g_prefix_error (error, "Couldn't AddMatch(%s): ", *r);
          return FALSE;
        }
      else
        {
          g_variant_unref (ret);
        }
    }

  return TRUE;
}

static gboolean
list_all_names (
    GDBusProxy *bus,
    GError **error)
{
  GVariant *ret;
  gchar **names;

  g_assert (G_IS_DBUS_PROXY (bus));

  ret = g_dbus_proxy_call_sync (bus, "ListNames", NULL,
      G_DBUS_CALL_FLAGS_NONE, -1, NULL, error);
  if (ret == NULL)
    {
      g_prefix_error (error, "Couldn't ListNames: ");
      return FALSE;
    }

  for (g_variant_get_child (ret, 0, "^a&s", &names);
       *names != NULL;
       names++)
    {
      gchar *name = *names;

      if (!g_dbus_is_unique_name (name) &&
          strcmp (name, "org.freedesktop.DBus") != 0)
        {
          GVariant *owner = g_dbus_proxy_call_sync (bus, "GetNameOwner",
              g_variant_new ("(s)", name),
              G_DBUS_CALL_FLAGS_NONE, -1, NULL, NULL);

          if (owner != NULL)
            g_variant_unref (owner);
          /* else they were too quick for us! */
        }
    }

  g_variant_unref (ret);
  return TRUE;
}

static gboolean
initable_init (
    GInitable *initable,
    GCancellable *cancellable,
    GError **error)
{
  BustlePcap *self = BUSTLE_PCAP (initable);
  BustlePcapPrivate *priv = self->priv;
  GDBusProxy *bus;

  if (priv->bus_type == G_BUS_TYPE_NONE)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_NOT_SUPPORTED,
          "Logging things other than message busses is not supported");
      return FALSE;
    }

  if (priv->filename == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_INVALID_ARGUMENT,
          "You must specify a filename");
      return FALSE;
    }

  /* FIXME: use DLT_DBUS when it makes it into libpcap. */
  priv->p = pcap_open_dead (DLT_NULL, 1 << 27);
  if (priv->p == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
          "pcap_open_dead failed. wtf");
      return FALSE;
    }

  priv->td.dumper = pcap_dump_open (priv->p, priv->filename);
  if (priv->td.dumper == NULL)
    {
      g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
          "Couldn't open pcap dump: %s", pcap_geterr (priv->p));
      return FALSE;
    }

  priv->thread = g_thread_create (log_thread, &priv->td, TRUE, error);
  if (priv->thread == NULL)
    {
      g_prefix_error (error, "Couldn't spawn logging thread: ");
      return FALSE;
    }

  priv->connection = g_bus_get_sync (priv->bus_type, NULL, error);
  if (priv->connection == NULL)
    {
      g_prefix_error (error, "Couldn't connect to %s bus: ",
          priv->bus_type == G_BUS_TYPE_SESSION ? "session" : "system");
      return FALSE;
    }

  priv->caps = g_dbus_connection_get_capabilities (priv->connection);

  bus = g_dbus_proxy_new_sync (priv->connection,
      G_DBUS_PROXY_FLAGS_DO_NOT_LOAD_PROPERTIES |
      G_DBUS_PROXY_FLAGS_DO_NOT_CONNECT_SIGNALS,
      NULL,
      "org.freedesktop.DBus",
      "/org/freedesktop/DBus",
      "org.freedesktop.DBus",
      NULL,
      error);
  if (bus == NULL)
    {
      g_prefix_error (error, "Couldn't construct bus proxy: ");
      return FALSE;
    }

  if (!match_everything (bus, error))
    return FALSE;

  priv->filter_id = g_dbus_connection_add_filter (priv->connection, filter,
      g_object_ref (self), g_object_unref);

  {
    /* FIXME: there's a race between listing all the names and binding to all
     * signals (and hence getting NameOwnerChanged). Old bustle-dbus-monitor had
     * it too.
     */
    gboolean ret = list_all_names (bus, error);
    g_object_unref (bus);

    return ret;
  }
}

/* FIXME: make this async? */
void
bustle_pcap_stop (
    BustlePcap *self)
{
  BustlePcapPrivate *priv = self->priv;

  if (priv->filter_id != 0)
    {
      g_return_if_fail (priv->connection != NULL);
      g_dbus_connection_remove_filter (priv->connection, priv->filter_id);
      priv->filter_id = 0;
    }

  if (priv->connection != NULL &&
      !g_dbus_connection_is_closed (priv->connection))
    {
      g_dbus_connection_close_sync (priv->connection, NULL, NULL);
    }

  if (priv->thread != NULL)
    {
      g_return_if_fail (priv->td.message_queue != NULL);
      /* Wait for the writer thread to spit out all the messages, then close up. */
      g_async_queue_push (priv->td.message_queue, STOP);
      g_thread_join (priv->thread);
      priv->thread = NULL;
    }

  if (priv->td.dumper != NULL)
    {
      pcap_dump_close (priv->td.dumper);
      priv->td.dumper = NULL;
    }

  if (priv->p != NULL)
    {
      pcap_close (priv->p);
      priv->p = NULL;
    }
}

static void
initable_iface_init (
    gpointer g_class,
    gpointer unused)
{
  GInitableIface *iface = g_class;

  iface->init = initable_init;
}

BustlePcap *
bustle_pcap_new (
    GBusType bus_type,
    const gchar *filename,
    gboolean verbose,
    GError **error)
{
  return g_initable_new (
      BUSTLE_TYPE_PCAP, NULL, error,
      "bus-type", bus_type,
      "filename", filename,
      "verbose", verbose,
      NULL);
}
