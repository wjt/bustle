/* -*- mode: C; c-file-style: "gnu"; indent-tabs-mode: nil; -*- */
/* dbus-monitor.c  Utility program to monitor messages on the bus
 *
 * Copyright (C) 2003 Philip Blundell <philb@gnu.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/* Modified by Will Thompson <will.thompson@collabora.co.uk> to include sender
 * in --profile output, and to attempt to use human-readable bus names where
 * possible.
 *
 * Modifications (C) 2008 Collabora Ltd. <http://www.collabora.co.uk>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DBUS_WIN
#include <winsock2.h>
#undef interface
#else
#include <sys/time.h>
#endif

#include <time.h>

#include <signal.h>

#include <dbus/dbus.h>

#include <glib.h>

#ifdef DBUS_WIN

/* gettimeofday is not defined on windows */
#define DBUS_SECONDS_SINCE_1601 11644473600LL
#define DBUS_USEC_IN_SEC        1000000LL

static int
gettimeofday (struct timeval *__p,
	      void *__t)
{
  union {
      unsigned long long ns100; /*time since 1 Jan 1601 in 100ns units */
      FILETIME           ft;
    } now;

  GetSystemTimeAsFileTime (&now.ft);
  __p->tv_usec = (long) ((now.ns100 / 10LL) % DBUS_USEC_IN_SEC);
  __p->tv_sec  = (long)(((now.ns100 / 10LL) / DBUS_SECONDS_SINCE_1601) - DBUS_SECONDS_SINCE_1601);

  return 0;
}
#endif

typedef struct _ReadableNameMapping ReadableNameMapping;
struct _ReadableNameMapping {
    char *unique_name;
    char *readable_name;
};

static GList *mappings;

const char *
lookup_name (const char *un)
{
  GList *l = mappings;

  while (l != NULL)
    {
      ReadableNameMapping *m = l->data;
      if (!strcmp (m->unique_name, un))
        return m->readable_name;

      l = l->next;
    }

  return NULL;
}

const char *
readable (const char *un)
{
  const char *r = lookup_name (un);

  if (r != NULL)
    return r;

  return un;
}

#define PROFILE_TIMED_FORMAT "%s\t%lu\t%lu"
#define TRAP_NULL_STRING(str) ((str) ? (str) : "<none>")

typedef enum
{
  PROFILE_ATTRIBUTE_FLAG_SERIAL = 1,
  PROFILE_ATTRIBUTE_FLAG_REPLY_SERIAL = 2,
  PROFILE_ATTRIBUTE_FLAG_SENDER = 4,
  PROFILE_ATTRIBUTE_FLAG_DESTINATION = 8,
  PROFILE_ATTRIBUTE_FLAG_PATH = 16,
  PROFILE_ATTRIBUTE_FLAG_INTERFACE = 32,
  PROFILE_ATTRIBUTE_FLAG_MEMBER = 64,
  PROFILE_ATTRIBUTE_FLAG_ERROR_NAME = 128,
} ProfileAttributeFlags;

static void
profile_print_with_attrs (const char *type, DBusMessage *message,
  struct timeval *t, ProfileAttributeFlags attrs)
{
  printf (PROFILE_TIMED_FORMAT, type, t->tv_sec, t->tv_usec);

  if (attrs & PROFILE_ATTRIBUTE_FLAG_SERIAL)
    printf ("\t%u", dbus_message_get_serial (message));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_REPLY_SERIAL)
    printf ("\t%u", dbus_message_get_reply_serial (message));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_SENDER)
    printf ("\t%s", TRAP_NULL_STRING (readable (dbus_message_get_sender (message))));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_DESTINATION)
    printf ("\t%s", TRAP_NULL_STRING (readable (dbus_message_get_destination (message))));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_PATH)
    printf ("\t%s", TRAP_NULL_STRING (dbus_message_get_path (message)));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_INTERFACE)
    printf ("\t%s", TRAP_NULL_STRING (dbus_message_get_interface (message)));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_MEMBER)
    printf ("\t%s", TRAP_NULL_STRING (dbus_message_get_member (message)));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_ERROR_NAME)
    printf ("\t%s", TRAP_NULL_STRING (dbus_message_get_error_name (message)));

  printf ("\n");
}

static void
print_message_profile (DBusMessage *message)
{
  struct timeval t;

  if (gettimeofday (&t, NULL) < 0)
    {
      printf ("un\n");
      return;
    }

  switch (dbus_message_get_type (message))
    {
      case DBUS_MESSAGE_TYPE_METHOD_CALL:
	profile_print_with_attrs ("mc", message, &t,
	  PROFILE_ATTRIBUTE_FLAG_SERIAL |
	  PROFILE_ATTRIBUTE_FLAG_SENDER |
	  PROFILE_ATTRIBUTE_FLAG_DESTINATION |
	  PROFILE_ATTRIBUTE_FLAG_PATH |
	  PROFILE_ATTRIBUTE_FLAG_INTERFACE |
	  PROFILE_ATTRIBUTE_FLAG_MEMBER);
	break;
      case DBUS_MESSAGE_TYPE_METHOD_RETURN:
	profile_print_with_attrs ("mr", message, &t,
	  PROFILE_ATTRIBUTE_FLAG_SERIAL |
	  PROFILE_ATTRIBUTE_FLAG_SENDER |
	  PROFILE_ATTRIBUTE_FLAG_DESTINATION |
	  PROFILE_ATTRIBUTE_FLAG_REPLY_SERIAL);
	break;
      case DBUS_MESSAGE_TYPE_ERROR:
	profile_print_with_attrs ("err", message, &t,
	  PROFILE_ATTRIBUTE_FLAG_SERIAL |
	  PROFILE_ATTRIBUTE_FLAG_SENDER |
	  PROFILE_ATTRIBUTE_FLAG_DESTINATION |
	  PROFILE_ATTRIBUTE_FLAG_REPLY_SERIAL);
	break;
      case DBUS_MESSAGE_TYPE_SIGNAL:
	profile_print_with_attrs ("sig", message, &t,
	  PROFILE_ATTRIBUTE_FLAG_SERIAL |
	  PROFILE_ATTRIBUTE_FLAG_SENDER |
	  PROFILE_ATTRIBUTE_FLAG_PATH |
	  PROFILE_ATTRIBUTE_FLAG_INTERFACE |
	  PROFILE_ATTRIBUTE_FLAG_MEMBER);
	break;
      default:
	printf (PROFILE_TIMED_FORMAT "\n", "tun", t.tv_sec, t.tv_usec);
	break;
    }
}

static DBusHandlerResult
profile_filter_func (DBusConnection	*connection,
		     DBusMessage	*message,
		     void		*user_data)
{
  print_message_profile (message);

  if (dbus_message_is_signal (message,
                              DBUS_INTERFACE_LOCAL,
                              "Disconnected"))
    exit (0);

  return DBUS_HANDLER_RESULT_HANDLED;
}

static void
usage (char *name, int ecode)
{
  fprintf (stderr, "Usage: %s [--system | --session] [watch expressions]\n", name);
  exit (ecode);
}

dbus_bool_t sigint_received = FALSE;

static void
sigint_handler (int signum)
{
  sigint_received = TRUE;
}

static void
get_well_known_names (DBusConnection *connection)
{
  DBusMessage *message = dbus_message_new_method_call ("org.freedesktop.DBus",
    "/org/freedesktop/DBus", "org.freedesktop.DBus", "ListNames");
  DBusError error;
  DBusMessage *ret;
  char **names;
  int i, n_names;

  dbus_error_init (&error);
  ret = dbus_connection_send_with_reply_and_block (connection, message, -1, &error);

  if (dbus_error_is_set (&error))
    {
      fprintf (stderr, "an error occurred: %s\n", error.message);
      dbus_error_free (&error);
      return;
    }

  dbus_message_get_args (ret, &error,
      DBUS_TYPE_ARRAY, DBUS_TYPE_STRING, &names, &n_names,
      DBUS_TYPE_INVALID);

  for (i = 0; i < n_names; i++)
    {
      ReadableNameMapping *mapping;
      DBusMessage *owner_ret;
      char *owner;
      const char *existing;

      if (*names[i] == ':')
        continue;

      message = dbus_message_new_method_call ("org.freedesktop.DBus",
          "/org/freedesktop/DBus", "org.freedesktop.DBus", "GetNameOwner");
      dbus_message_append_args (message,
          DBUS_TYPE_STRING, names + i,
          DBUS_TYPE_INVALID);

      owner_ret = dbus_connection_send_with_reply_and_block (connection,
          message, -1, &error);

      if (dbus_error_is_set (&error))
        {
          fprintf (stderr, "GetNameOwner failed: %s\n", error.message);
          dbus_error_free (&error);
          return;
        }

      dbus_message_get_args (owner_ret, &error,
          DBUS_TYPE_STRING, &owner,
          DBUS_TYPE_INVALID);

      if (dbus_error_is_set (&error))
        {
          fprintf (stderr, "get_args(GetNameOwner) failed: %s\n", error.message);
          dbus_error_free (&error);
          return;
        }

      existing = lookup_name (owner);

      if (existing == NULL || strlen (existing) > strlen (names[i]))
        {
          mapping = malloc (sizeof (ReadableNameMapping));
          mapping->unique_name = owner;
          mapping->readable_name = names[i];

          mappings = g_list_prepend (mappings, mapping);
        }
    }
}


int
main (int argc, char *argv[])
{
  DBusConnection *connection;
  DBusError error = DBUS_ERROR_INIT;
  DBusBusType type = DBUS_BUS_SESSION;
  DBusHandleMessageFunction filter_func = profile_filter_func;

  int i = 0, j = 0, numFilters = 0;
  char **filters = NULL;
  for (i = 1; i < argc; i++)
    {
      char *arg = argv[i];

      if (!strcmp (arg, "--system"))
	type = DBUS_BUS_SYSTEM;
      else if (!strcmp (arg, "--session"))
	type = DBUS_BUS_SESSION;
      else if (!strcmp (arg, "--help"))
	usage (argv[0], 0);
      else if (!strcmp (arg, "--"))
	continue;
      else if (arg[0] == '-')
	usage (argv[0], 1);
      else {
	numFilters++;
       filters = (char **)realloc(filters, numFilters * sizeof(char *));
	filters[j] = (char *)malloc((strlen(arg) + 1) * sizeof(char *));
	snprintf(filters[j], strlen(arg) + 1, "%s", arg);
	j++;
      }
    }

  dbus_error_init (&error);
  connection = dbus_bus_get (type, &error);
  if (connection == NULL)
    {
      fprintf (stderr, "Failed to open connection to %s message bus: %s\n",
	       (type == DBUS_BUS_SYSTEM) ? "system" : "session",
               error.message);
      dbus_error_free (&error);
      exit (1);
    }

  get_well_known_names (connection);

  if (numFilters)
    {
      for (i = 0; i < j; i++)
        {
          dbus_bus_add_match (connection, filters[i], &error);
          if (dbus_error_is_set (&error))
            {
              fprintf (stderr, "Failed to setup match \"%s\": %s\n",
                       filters[i], error.message);
              dbus_error_free (&error);
              exit (1);
            }
	  free(filters[i]);
        }
    }
  else
    {
      dbus_bus_add_match (connection,
		          "type='signal'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
      dbus_bus_add_match (connection,
		          "type='method_call'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
      dbus_bus_add_match (connection,
		          "type='method_return'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
      dbus_bus_add_match (connection,
		          "type='error'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
    }

  if (!dbus_connection_add_filter (connection, filter_func, NULL, NULL)) {
    fprintf (stderr, "Couldn't add filter!\n");
    exit (1);
  }

  /* we handle SIGINT so exit() is reached and flushes stdout */
  signal (SIGINT, sigint_handler);
  while (dbus_connection_read_write_dispatch(connection, -1)
          && !sigint_received)
    ;
  exit (0);
 lose:
  fprintf (stderr, "Error: %s\n", error.message);
  exit (1);
}

