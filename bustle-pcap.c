/* bustle-pcap.c: utility to log D-Bus traffic to a pcap file.
 *
 * Copyright © 2011 Will Thompson <will@willthompson.co.uk>
 *
 * This program is free software; you can redistribute it and/or
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <string.h>
#include <pcap/pcap.h>
#include <glib.h>
#include <gio/gio.h>
#include <gio/gunixinputstream.h>

#define DIE_IF_NULL(_x, ...) \
  G_STMT_START { \
    if (_x == NULL) \
      { \
        fprintf (stderr, __VA_ARGS__); \
        exit (1); \
      } \
  } G_STMT_END

GDBusCapabilityFlags caps;

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

GDBusMessage *
filter (
    GDBusConnection *connection,
    GDBusMessage *message,
    gboolean incoming,
    gpointer user_data)
{
  GAsyncQueue *message_queue = user_data;
  Message m;
  GError *error = NULL;

  gettimeofday (&m.ts, NULL);
  m.blob = g_dbus_message_to_blob (message, &m.size, caps, &error);
  DIE_IF_NULL (m.blob, "Couldn't marshal message: %s", error->message);
  g_async_queue_push (message_queue, g_slice_dup (Message, &m));

  /* We have to say we've handled the message, or else GDBus replies to other
   * people's method calls and we all get really confused.
   */
  return NULL;
}

static gboolean
stdin_func (
    GPollableInputStream *g_stdin,
    GMainLoop *loop)
{
  char buf[2];

  if (g_pollable_input_stream_read_nonblocking (g_stdin, buf, 1, NULL, NULL)
      != -1)
    {
      g_main_loop_quit (loop);
      return FALSE;
    }

  return TRUE;
}

static void
let_me_quit (GMainLoop *loop)
{
  /* FIXME: also handle ^C. glib 2.29 makes this easy. */
  GInputStream *g_stdin = g_unix_input_stream_new (0, FALSE);
  GSource *source = g_pollable_input_stream_create_source (
      G_POLLABLE_INPUT_STREAM (g_stdin), NULL);

  g_source_set_callback (source, (GSourceFunc) stdin_func, loop, NULL);
  g_source_attach (source, NULL);
}

static void
match_everything (GDBusConnection *connection)
{
  char *rules[] = {
      "type='signal'",
      "type='method_call'",
      "type='method_return'",
      "type='error'",
      NULL
  };
  char **r;
  GError *error = NULL;
  GDBusProxy *bus = g_dbus_proxy_new_sync (connection,
      G_DBUS_PROXY_FLAGS_DO_NOT_LOAD_PROPERTIES |
      G_DBUS_PROXY_FLAGS_DO_NOT_CONNECT_SIGNALS,
      NULL,
      "org.freedesktop.DBus",
      "/org/freedesktop/DBus",
      "org.freedesktop.DBus",
      NULL,
      &error);
  DIE_IF_NULL (bus, "Couldn't construct bus proxy: %s\n", error->message);

  for (r = rules; *r != NULL; r++)
    {
      GVariant *ret = g_dbus_proxy_call_sync (
          bus,
          "AddMatch",
          g_variant_new ("(s)", *r),
          G_DBUS_CALL_FLAGS_NONE,
          -1,
          NULL,
          &error);
      DIE_IF_NULL (ret, "Couldn't AddMatch(%s): %s\n", *r, error->message);
    }

  g_object_unref (bus);
}

int
main (
    int argc,
    char **argv)
{
  GMainLoop *loop;
  GDBusConnection *connection;
  GThread *thread;
  ThreadData td;
  pcap_t *p;
  guint filter_id;
  GError *error = NULL;

  g_type_init ();

  if (argc != 2)
    {
      fprintf (stderr, "Usage: bustle-pcap OUTPUT-FILE\n");
      return 2;
    }

  /* FIXME: use DLT_DBUS when it makes it into libpcap. */
  p = pcap_open_dead (DLT_NULL, 1 << 27);
  DIE_IF_NULL (p, "pcap_open_dead failed. wtf");

  td.dumper = pcap_dump_open (p, argv[1]);
  DIE_IF_NULL (td.dumper, "Couldn't open pcap dump: %s\n", pcap_geterr (p));

  td.message_queue = g_async_queue_new ();

  thread = g_thread_create (log_thread, &td, TRUE, &error);
  DIE_IF_NULL (thread, "Couldn't spawn logging thread: %s\n", error->message);

  connection = g_bus_get_sync (G_BUS_TYPE_SESSION, NULL, &error);
  DIE_IF_NULL (connection, "Couldn't connect to session bus: %s\n",
      error->message);

  caps = g_dbus_connection_get_capabilities (connection);
  match_everything (connection);

  filter_id = g_dbus_connection_add_filter (connection, filter,
      g_async_queue_ref (td.message_queue),
      (GDestroyNotify) g_async_queue_unref);

  loop = g_main_loop_new (NULL, FALSE);
  let_me_quit (loop);
  g_main_loop_run (loop);
  g_main_loop_unref (loop);

  g_dbus_connection_remove_filter (connection, filter_id);
  g_dbus_connection_close_sync (connection, NULL, NULL);

  /* Wait for the writer thread to spit out all the messages, then close up. */
  g_async_queue_push (td.message_queue, STOP);
  g_thread_join (thread);
  pcap_dump_close (td.dumper);
  pcap_close (p);

  return 0;
}
