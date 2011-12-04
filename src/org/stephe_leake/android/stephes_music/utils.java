//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011 Stephen Leake.  All Rights Reserved.
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
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.ContextWrapper;
import android.content.Intent;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.net.Uri;
import android.os.RemoteException;
import android.text.format.Time;
import android.util.Log;
import android.widget.Toast;

import java.io.PrintWriter;
import java.lang.Thread;
import java.util.Formatter;
import java.util.HashMap;
import java.util.Locale;

public class utils {

   //  Notification messages to user views, sent via Intent. Alphabetical order
   public static final String META_CHANGED = "org.stephe_leake.android.stephes_music.metachanged";
   //  artist   string
   //  album    string
   //  track    string
   //  duration string (?)
   //  playlist string (name pos/count)

   public static final String PLAYSTATE_CHANGED = "org.stephe_leake.android.stephes_music.playstatechanged";
   //  playing  boolean

   //  Commands to server via Intents sent via broadcast. Alphabetical order
   public static final String INTENT_NEXT = "org.stephe_leake.Android.music_player.intent.next";
   public static final String INTENT_PAUSE = "org.stephe_leake.Android.music_player.intent.pause";
   public static final String INTENT_PLAYLIST = "org.stephe_leake.Android.music_player.intent.playlist";
   public static final String INTENT_PREVIOUS = "org.stephe_leake.Android.music_player.intent.previous";
   public static final String INTENT_SEEK = "org.stephe_leake.Android.music_player.intent.seek";
   public static final String INTENT_TOGGLEPAUSE =     "org.stephe_leake.Android.music_player.intent.togglepause";
   public static final String INTENT_UPDATE_DISPLAY = "org.stephe_leake.Android.music_player.intent.update_display";


   private static StringBuilder sFormatBuilder = new StringBuilder();
   private static Formatter sFormatter = new Formatter(sFormatBuilder, Locale.getDefault());
   // FIXME: compare to android.text.Formatter, android.text.format.time

   private static final Object[] sTimeArgs = new Object[5];

   public static String makeTimeString(Context context, long millisecs)
   {
      final long secs = millisecs / 1000;
      String durationformat = context.getString
         (secs < 3600 ? R.string.durationformatshort : R.string.durationformatlong);

      /* Provide multiple arguments so the format can be changed easily
       * by modifying the xml.
       */
      sFormatBuilder.setLength(0);

      final Object[] timeArgs = sTimeArgs;

      timeArgs[0] = secs / 3600; // hours
      timeArgs[1] = secs / 60;  // minutes
      timeArgs[2] = (secs / 60) % 60; // minutes_in_hour
      timeArgs[3] = secs;       // seconds
      timeArgs[4] = secs % 60;  // seconds_in_minute

      return sFormatter.format(durationformat, timeArgs).toString();
   }

   static int getCardId(Context context) {
      ContentResolver res = context.getContentResolver();
      Cursor c = res.query(Uri.parse("content://media/external/fs_id"), null, null, null, null);
      int id = -1;
      if (c != null) {
         c.moveToFirst();
         id = c.getInt(0);
         c.close();
      }
      return id;
   }

   static class LogEntry {
      Object item;
      long time;

      LogEntry(Object o) {
         item = o;
         time = System.currentTimeMillis();
      }

      void dump(PrintWriter out) {
         sTime.set(time);
         out.print(sTime.toString() + " : ");
         if (item instanceof Exception) {
            ((Exception)item).printStackTrace(out);
         } else {
            out.println(item);
         }
      }
   }

   private static LogEntry[] sMusicLog = new LogEntry[100];
   private static int sLogPtr = 0;
   private static Time sTime = new Time();

   public static void debugLog(Object o)
   {
      // Cache error messages to be dumped by debugDump.
      //
      // This is better than just Log, because Log messages are dumped
      // by 'adb logcat', while these are dumped by 'adb shell dumpsys
      // activity service Stephes_Music_Service'. The former shows
      // messages from all activities and services; the later just
      // from this log.
      //
      // If 'o' is an Exception, it will include a stack trace.

      sMusicLog[sLogPtr] = new LogEntry(o);
      sLogPtr++;
      if (sLogPtr >= sMusicLog.length) {
         sLogPtr = 0;
      }
   }

   static void debugDump(PrintWriter out) {
      for (int i = 0; i < sMusicLog.length; i++) {
         int idx = (sLogPtr + i);
         if (idx >= sMusicLog.length) {
            idx -= sMusicLog.length;
         }
         LogEntry entry = sMusicLog[idx];
         if (entry != null) {
            entry.dump(out);
         }
      }
   }

   final public static String Log_App_Name = "stephes_music_player"; // for use in log messages

   public static void Error_Log(Context context, String msg)
   {
      debugLog(msg);
      Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
   }

   static void Info_Log(Context context, String msg)
   {
      debugLog(msg);
      Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
   }

}
