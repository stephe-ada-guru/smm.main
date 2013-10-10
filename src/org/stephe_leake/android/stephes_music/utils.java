//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011, 2012, 2013 Stephen Leake.  All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or
//  modify it under terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 3, or
//  (at your option) any later version. This program is distributed in
//  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
//  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
//  PARTICULAR PURPOSE. See the GNU General Public License for more
//  details. You should have received a copy of the GNU General Public
//  License distributed with this program; see file COPYING. If not,
//  write to the Free Software Foundation, 51 Franklin Street, Suite
//  500, Boston, MA 02110-1335, USA.

package org.stephe_leake.android.stephes_music;

import android.app.Activity;
import android.content.Context;
import android.text.format.Time;
import android.util.Log;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;

import java.io.PrintWriter;
import java.lang.RuntimeException;

public class utils
{

   public static final String serviceClassName =
      "org.stephe_leake.android.stephes_music.service";

   //  Notification messages to user views, sent via Intent. Alphabetical order
   public static final String META_CHANGED = "org.stephe_leake.android.stephes_music.metachanged";
   //  artist   string
   //  album    string
   //  track    string
   //  duration int (milliseconds)
   //  playlist string (name pos/count)

   public static final String PLAYSTATE_CHANGED = "org.stephe_leake.android.stephes_music.playstatechanged";
   //  playing  boolean
   //  position int (milliseconds)

   //  Commands to server via Intent actions sent via broadcast. Alphabetical order
   //  Only one intent, so we can add commands without adding to the reciever filter.
   public static final String ACTION_COMMAND         = "org.stephe_leake.android.stephes_music.action.command";
   public static final String COMMAND_DUMP_LOG       = "dump_log";
   public static final String COMMAND_NEXT           = "next";
   public static final String COMMAND_NOTE           = "note";
   public static final String COMMAND_PAUSE          = "pause";
   public static final String COMMAND_PLAY           = "play";
   public static final String COMMAND_PLAYLIST       = "playlist";
   // playlist  string (abs file name)
   public static final String COMMAND_PREVIOUS       = "org.stephe_leake.android.stephes_music.command.previous";
   public static final String COMMAND_SAVE_STATE     = "org.stephe_leake.android.stephes_music.command.save_state";
   public static final String COMMAND_SEEK           = "org.stephe_leake.android.stephes_music.command.seek";
   // position  int (milliseconds)
   public static final String COMMAND_TOGGLEPAUSE    = "org.stephe_leake.android.stephes_music.command.togglepause";
   public static final String COMMAND_UPDATE_DISPLAY = "org.stephe_leake.android.stephes_music.command.update_display";

   // sub-activity result codes
   public static final int RESULT_TEXT_SCALE = Activity.RESULT_FIRST_USER + 1;

   // methods

   public static TextView findTextViewById (Activity a, int id)
   {
      final View v = a.findViewById(id);

      if (v == null) throw new RuntimeException("no such id " + id);

      if (v instanceof TextView)
      {
         return (TextView)v;
      }
      else
      {
         throw new RuntimeException(id + " is not a TextView; it is a " + v.toString());
      }
   }

   public static String makeTimeString(Context context, long millisecs)
   {
      final Time time     = new Time();
      final long oneHour  = 3600 * 1000; // milliseconds
      final String format = context.getString
         (millisecs < oneHour ? R.string.durationformatshort : R.string.durationformatlong);

      time.set(millisecs);
      return time.format(format);
   }

   static class LogEntry
   {
      Object item;

      LogEntry(Object o) {item = o;}

      void dump(PrintWriter out)
      {
         if (item instanceof Exception)
         {
            out.println(item);
            ((Exception)item).printStackTrace(out);
         }
         else
         {
            out.println(item);
         }
      }
   }

   private static LogEntry[] log = new LogEntry[100];

   private static int logNext = 0;

   public static void debugClear()
   {
      for (int i = 0; i < log.length; i++)
      {
         log[i] = null;
      }
      logNext = 0;
   }

   public static void debugLog(Object o)
   {
      // Cache error messages to be dumped by debugDump, which is
      // called by 'adb shell dumpsys activity service ...service' and
      // activity menu "dump log".
      //
      // However, this log disappears if the service dies. FIXME: need
      // 'dump log on service die' option.
      //
      // If 'o' is an Exception, the dump will include a stack trace.

      log[logNext++] = new LogEntry(o);
      if (logNext >= log.length)
      {
         logNext = 0;
      }
   }

   public static void debugDump(PrintWriter out)
   {
      for (int i = 0; i < log.length; i++)
      {
         int idx = (logNext + i);
         if (idx >= log.length)
         {
            idx -= log.length;
         }
         LogEntry entry = log[idx];
         if (entry != null)
         {
            entry.dump(out);
         }
      }
   }

   public static void errorLog(Context context, String msg, Throwable e)
   {
      // programmer errors (possibly due to Android bugs :)
      Log.e(serviceClassName, msg, e);
      Toast.makeText(context, msg + e.toString(), Toast.LENGTH_LONG).show();
   }

   public static void errorLog(Context context, String msg)
   {
      // programmer errors (possibly due to Android bugs :)
      Log.e(serviceClassName, msg);
      Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
   }

   static void infoLog(Context context, String msg)
   {
      // helpful user messages, ie "could not play"
      Log.i(serviceClassName, msg);
      Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
   }

   static void verboseLog(String msg)
   {
      // for post-mortem debugging
      Log.v(serviceClassName, msg);
   }

}
