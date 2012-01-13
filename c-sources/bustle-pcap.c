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

#include <string.h>
#include <glib.h>
#include <glib-unix.h>
#include <glib/gprintf.h>
#include <gio/gunixinputstream.h>

#include "pcap-monitor.h"

static gboolean verbose = FALSE;
static gboolean quiet = FALSE;

#if GLIB_CHECK_VERSION (2, 30, 0)
static void
let_me_quit (GMainLoop *loop)
{
  g_unix_signal_add (SIGINT, (GSourceFunc) g_main_loop_quit, loop);

  if (!quiet)
    g_printf ("Hit Control-C to stop logging.\n");
}
#else
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
  GInputStream *g_stdin = g_unix_input_stream_new (0, FALSE);
  GSource *source = g_pollable_input_stream_create_source (
      G_POLLABLE_INPUT_STREAM (g_stdin), NULL);

  g_source_set_callback (source, (GSourceFunc) stdin_func, loop, NULL);
  g_source_attach (source, NULL);
  g_printf ("Hit Enter to stop logging. (Do not hit Control-C.)\n");
}
#endif

static gboolean session_specified = FALSE;
static gboolean system_specified = FALSE;
static gchar **filenames = NULL;

static GOptionEntry entries[] = {
    { "session", 'e', 0, G_OPTION_ARG_NONE, &session_specified,
      "Monitor session bus (default)", NULL
    },
    { "system", 'y', 0, G_OPTION_ARG_NONE, &system_specified,
      "Monitor system bus", NULL
    },
    { "verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose,
      "Print brief summaries of captured messages to stdout", NULL
    },
    { "quiet", 'q', 0, G_OPTION_ARG_NONE, &quiet,
      "Don't print out instructions", NULL
    },
    { G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &filenames,
      "blah blah", NULL
    },
    { NULL }
};

static void
parse_arguments (
    int *argc,
    char ***argv,
    GBusType *bus_type,
    gchar **filename)
{
  GOptionContext *context;
  gchar *usage;
  GError *error = NULL;
  gboolean ret;

  context = g_option_context_new ("FILENAME - logs D-Bus traffic to FILENAME");
  g_option_context_add_main_entries (context, entries, NULL);

  ret = g_option_context_parse (context, argc, argv, &error);
  usage = g_option_context_get_help (context, TRUE, NULL);

  if (!ret)
    {
      fprintf (stderr, "%s\n", error->message);
      fprintf (stderr, "%s", usage);
      exit (2);
    }

  if (session_specified && system_specified)
    {
      fprintf (stderr, "You may only specify one of --session and --system\n");
      fprintf (stderr, "%s", usage);
      exit (2);
    }
  else if (system_specified)
    {
      *bus_type = G_BUS_TYPE_SYSTEM;
    }
  else
    {
      *bus_type = G_BUS_TYPE_SESSION;
    }

  if (filenames == NULL ||
      filenames[0] == NULL ||
      filenames[1] != NULL)
    {
      fprintf (stderr, "You must specify exactly one output filename\n");
      fprintf (stderr, "%s", usage);
      exit (2);
    }

  *filename = filenames[0];
}

static void
message_logged_cb (
    BustlePcapMonitor *pcap,
    GDBusMessage *message,
    gboolean is_incoming,
    guint8 *data,
    guint len,
    gpointer user_data)
{
    g_print ("(%s) %s -> %s: %u %s\n",
        is_incoming ? "incoming" : "outgoing",
        g_dbus_message_get_sender (message),
        g_dbus_message_get_destination (message),
        g_dbus_message_get_message_type (message),
        g_dbus_message_get_member (message));
}

int
main (
    int argc,
    char **argv)
{
  GMainLoop *loop;
  GBusType bus_type;
  gchar *filename;
  GError *error = NULL;
  BustlePcapMonitor *pcap;

  g_type_init ();
  parse_arguments (&argc, &argv, &bus_type, &filename);

  pcap = bustle_pcap_monitor_new (bus_type, filename, &error);
  if (pcap == NULL)
    {
      fprintf (stderr, "%s", error->message);
      exit (1);
    }

  if (verbose)
    g_signal_connect (pcap, "message-logged",
        G_CALLBACK (message_logged_cb), NULL);

  loop = g_main_loop_new (NULL, FALSE);

  if (!quiet)
    g_printf ("Logging D-Bus traffic to '%s'...\n", filename);

  let_me_quit (loop);
  g_main_loop_run (loop);
  g_main_loop_unref (loop);

  bustle_pcap_monitor_stop (pcap);
  g_object_unref (pcap);

  return 0;
}
