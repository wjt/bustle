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

#include <dbus/dbus.h>

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

#define PROFILE_TIMED_FORMAT "%s\t%lu\t%lu"
#define TRAP_NULL_STRING(str) ((str) ? (str) : "<none>")

#define EAVESDROPPING_RULE "eavesdrop=true"

inline static void
oom (const char *doing)
{
    fprintf (stderr, "OOM while %s\n", doing);
      exit (1);
}

static void
print_name_owner_changed (struct timeval *t,
    const char *name,
    const char *old_owner,
    const char *new_owner)
{
  if (!strcmp (name, "org.freedesktop.DBus"))
    return;

  printf (PROFILE_TIMED_FORMAT, "nameownerchanged", t->tv_sec, t->tv_usec);

  /* Use '!' to represent "no name here". */
  printf ("\t%s\t%s\t%s\n", name,
    (*old_owner == '\0' ? "!" : old_owner),
    (*new_owner == '\0' ? "!" : new_owner));
}

static void
name_owner_changed_cb (DBusMessage *message)
{
  char *name, *old_owner, *new_owner;
  DBusError error;
  struct timeval t;

  if (gettimeofday (&t, NULL) < 0)
    {
      printf ("un\n");
      return;
    }

  dbus_error_init (&error);
  dbus_message_get_args (message, &error,
      DBUS_TYPE_STRING, &name,
      DBUS_TYPE_STRING, &old_owner,
      DBUS_TYPE_STRING, &new_owner,
      DBUS_TYPE_INVALID);

  if (dbus_error_is_set (&error))
    {
      fprintf (stderr, "name_owner_changed_cb: %s\n", error.message);
      dbus_error_free (&error);
      return;
    }

  print_name_owner_changed (&t, name, old_owner, new_owner);
}

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
    printf ("\t%s", TRAP_NULL_STRING (dbus_message_get_sender (message)));

  if (attrs & PROFILE_ATTRIBUTE_FLAG_DESTINATION)
    printf ("\t%s", TRAP_NULL_STRING (dbus_message_get_destination (message)));

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

  if (dbus_message_is_signal (message, DBUS_INTERFACE_DBUS, "NameOwnerChanged"))
    name_owner_changed_cb (message);
  else
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

static void
get_well_known_names (DBusConnection *connection)
{
  DBusMessage *message = dbus_message_new_method_call ("org.freedesktop.DBus",
    "/org/freedesktop/DBus", "org.freedesktop.DBus", "ListNames");
  DBusError error;
  DBusMessage *ret;
  char **names;
  int i, n_names;
  struct timeval t = { 0, 0 };

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

  /* First, print the unique names */
  for (i = 0; i < n_names; i++)
    if (*names[i] == ':')
        print_name_owner_changed (&t, names[i], "", names[i]);

  /* Now print the well-known names */
  for (i = 0; i < n_names; i++)
    {
      DBusMessage *owner_ret;
      char *owner;

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

      print_name_owner_changed (&t, names[i], "", owner);
    }

  dbus_free_string_array (names);
}


typedef enum {
    FEATURE_UNKNOWN = 0,
    FEATURE_PRESENT,
    FEATURE_NOT_PRESENT
} FeatureDetected;

char *prepend_eavesdrop_keyword (const char *filter)
{
  unsigned int eavesdrop_filter_len;
  char *new_filter;

  eavesdrop_filter_len = strlen (EAVESDROPPING_RULE) + 1 + \
                         strlen (filter) + 1;

  new_filter = malloc (eavesdrop_filter_len * sizeof(char *));
  if (new_filter == NULL)
    oom ("creating rule with eavesdrop=true prepended");

  snprintf(new_filter, eavesdrop_filter_len, "%s,%s",
      EAVESDROPPING_RULE, filter);

  return new_filter;
}

/* check support for eavesdrop=true in filter and use the the right filter
 * rules set according on whether the feature is supported */
void bustle_bus_add_match (DBusConnection *connection,
    const char *filter,
    DBusError *error)
{
  static FeatureDetected feat_detected = FEATURE_UNKNOWN;

  if (feat_detected == FEATURE_PRESENT)
    {
      /* silently prepend eavesdrop=true keyword to user rule, it doesn't
       * matter if user has added it in @filter also, while it will be
       * disabled if he/she added eavesdrop=false */
      char *tmp_filter;
      
      tmp_filter = prepend_eavesdrop_keyword (filter);
      dbus_bus_add_match (connection, tmp_filter, error);

      free (tmp_filter);
    }
  else if (feat_detected == FEATURE_NOT_PRESENT)
    {
      /* do not touch anything */
      dbus_bus_add_match (connection, filter, error);
    }
  else /* FEATURE_UNKNOWN -> yet to discover */
    {
      char *tmp_filter;

      tmp_filter = prepend_eavesdrop_keyword (filter);
      dbus_bus_add_match (connection, tmp_filter, error);
      if (dbus_error_is_set (error) &&
          strcmp (error->name, DBUS_ERROR_MATCH_RULE_INVALID) == 0)
        {
          dbus_error_free (error);

          /* using a dbus version witout this feature, too bad, but still OK */
          feat_detected = FEATURE_NOT_PRESENT;
          /* re-call myself, now I know what to do (unless the user's rule is
           * malformed) */
          bustle_bus_add_match (connection, filter, error);
          if (dbus_error_is_set (error))
            /* probably the filter is malformed, rollback to UKNOWN and pass
             * the error to the caller */
            feat_detected = FEATURE_UNKNOWN;
        }
      else if (!dbus_error_is_set (error)) /* we support it! */
        {
          feat_detected = FEATURE_PRESENT;
        }

      /* the case when @error is set but it's not
       * DBUS_ERROR_MATCH_RULE_INVALID is left to be handled by the caller, we
       * cannot take a decision under this situation. leaving feat_detected
       * untouched and passing the error to the user*/

      free (tmp_filter);
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

  setvbuf(stdout, NULL, _IOLBF, 0);

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
          filters = (char **) realloc (filters, numFilters * sizeof (char *));
          if (filters == NULL)
            oom ("(re)creating filter slots");
          filters[j] = (char *) malloc ((strlen (arg) + 1) * sizeof (char *));
          if (filters[j] == NULL)
            oom ("creating filter");
          snprintf (filters[j], strlen(arg) + 1, "%s", arg);
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
          bustle_bus_add_match (connection, filters[i], &error);
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
      bustle_bus_add_match (connection,
		          "type='signal'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
      bustle_bus_add_match (connection,
		          "type='method_call'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
      bustle_bus_add_match (connection,
		          "type='method_return'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
      bustle_bus_add_match (connection,
		          "type='error'",
		          &error);
      if (dbus_error_is_set (&error))
        goto lose;
    }

  if (!dbus_connection_add_filter (connection, filter_func, NULL, NULL)) {
    fprintf (stderr, "Couldn't add filter!\n");
    exit (1);
  }

  while (dbus_connection_read_write_dispatch(connection, 1000))
    ;
  exit (0);
 lose:
  fprintf (stderr, "Error: %s\n", error.message);
  exit (1);
}

