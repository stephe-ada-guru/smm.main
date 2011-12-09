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

   public static final String serviceClassName =
      "org.stephe_leake.android.stephes_music.service";

   //  Notification messages to user views, sent via Intent. Alphabetical order
   public static final String META_CHANGED = "org.stephe_leake.android.stephes_music.metachanged";
   //  artist   string
   //  album    string
   //  track    string
   //  duration string (?)
   //  playlist string (name pos/count)

   public static final String PLAYSTATE_CHANGED = "org.stephe_leake.android.stephes_music.playstatechanged";
   //  playing  boolean
   //  FIXME: add current pos

   //  Commands to server via Intent actions sent via broadcast. Alphabetical order
   public static final String ACTION_NEXT = "org.stephe_leake.android.stephes_music.action.next";
   public static final String ACTION_PAUSE = "org.stephe_leake.android.stephes_music.action.pause";
   public static final String ACTION_PLAYLIST = "org.stephe_leake.android.stephes_music.action.playlist";
   // playlist  string (abs file name)
   public static final String ACTION_PREVIOUS = "org.stephe_leake.android.stephes_music.action.previous";
   public static final String ACTION_SEEK = "org.stephe_leake.android.stephes_music.action.seek";
   public static final String ACTION_TOGGLEPAUSE =     "org.stephe_leake.android.stephes_music.action.togglepause";
   public static final String ACTION_UPDATE_DISPLAY = "org.stephe_leake.android.stephes_music.action.update_display";


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
         out.print(sTime.format2445() + " : ");
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

   public static void debugClear()
   {
      for (int i = 0; i < sMusicLog.length; i++)
      {
         sMusicLog[i] = null;
      }
      sLogPtr = 0;
   }

   public static void debugLog(Object o)
   {
      // Cache error messages to be dumped by debugDump.
      //
      // This is better than just Log, because Log messages are dumped
      // by 'adb logcat', while these are dumped by 'adb shell dumpsys
      // activity service service'. The former shows messages from all
      // activities and services; the later just from this log.
      //
      // However, this log disappears if the service dies.
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

   public static void errorLog(Context context, String msg)
   {
      debugLog(msg);
      Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
   }

   static void infoLog(Context context, String msg)
   {
      debugLog(msg);
      Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
   }

}
