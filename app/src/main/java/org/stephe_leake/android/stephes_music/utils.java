//  Abstract :
//
//  misc stuff
//
//  Copyright (C) 2011 - 2013, 2015 - 2018 Stephen Leake.  All Rights Reserved.
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
import android.app.AlertDialog;
import android.content.Intent;
import android.app.PendingIntent;
import android.content.Context;
import android.text.format.Time;
import android.util.Log;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;

import java.io.PrintWriter;
import java.lang.RuntimeException;
import java.text.SimpleDateFormat;
import java.io.File;
import java.io.FileWriter;
import java.util.Locale;

public class utils
{

   public static final long millisPerMinute = 60 * 1000;
   public static final long millisPerHour   = 60 * millisPerMinute;
   public static final long millisPerDay    = 24 * millisPerHour;

   public static final String preferencesName = "stephes_music";

   //  Notification ids; all with null tag
   public static final int notif_play_id     = 1;
   public static final int notif_download_id = 2;


   //  Messages to user views, sent via Intent. Alphabetical order
   public static final String META_CHANGED = "org.stephe_leake.android.stephes_music.metachanged";
   //  get artist, album, title, albumArt from utils.retriever.
   //  extras:
   //  duration string (milliseconds)
   //  playlist string (name pos/count)

   public static final String PLAYSTATE_CHANGED = "org.stephe_leake.android.stephes_music.playstatechanged";
   //  playing  boolean
   //  position int (milliseconds)

   //  Commands to server via Intent actions sent via broadcast. Alphabetical order
   //  Only one action, so we can add commands without adding to the reciever filter.
   public static final String ACTION_COMMAND = "org.stephe_leake.android.stephes_music.action.command";
   // according to android docs, extra field names must inlude the package prefix (no explanation of why)
   public static final String EXTRA_COMMAND = "org.stephe_leake.android.stephes_music.extra.command";
   public static final String EXTRA_COMMAND_POSITION =
      "org.stephe_leake.android.stephes_music.action.command_position";
   public static final String EXTRA_COMMAND_PLAYLIST =
      "org.stephe_leake.android.stephes_music.action.command_playlist";
   public static final String EXTRA_COMMAND_STATE =
      "org.stephe_leake.android.stephes_music.action.command_state";

   // values for extras; alphabetical. We'd like to use an enum here,
   // but we can't make that parcelable for intent extras.
   public static final int COMMAND_RESERVED = 1; // something sends this to PlayService!

   public static final int COMMAND_DOWNLOAD           = 2;
   public static final int COMMAND_JUMP               = 4;
   public static final int COMMAND_NEXT               = 5;
   public static final int COMMAND_NOTE               = 6;
   public static final int COMMAND_PAUSE              = 7;
   public static final int COMMAND_PLAY               = 8;
   public static final int COMMAND_PLAYLIST           = 9; // playlist  string (abs file name)
   public static final int COMMAND_PREVIOUS           = 10;
   public static final int COMMAND_QUIT               = 11;
   public static final int COMMAND_RESET_PLAYLIST     = 12;
   public static final int COMMAND_SAVE_STATE         = 13;
   public static final int COMMAND_SEEK               = 14; // position  int (milliseconds)
   public static final int COMMAND_TOGGLEPAUSE        = 15;
   public static final int COMMAND_UPDATE_DISPLAY     = 16;

   // sub-activity result codes
   public static final int RESULT_TEXT_SCALE         = Activity.RESULT_FIRST_USER + 1;

   public static final int pauseIntentId           = 1;
   public static final int playIntentId            = 2;
   public static final int prevIntentId            = 3;
   public static final int nextIntentId            = 4;
   public static final int activityIntentId        = 5;
   public static final int showDownloadLogIntentId = 6;
   public static final int showErrorLogIntentId    = 7;

   ////////// Shared objects

   public static MetaData retriever;

   public static String smmDirectory;
   // Absolute path to directory containing playlist files and files
   // used to interface with Stephe's Music manager (smm); .last
   // files, notes files.
   //
   // Set by Activity to getExternalStorageDir().

   public static String playlistBasename;
   // Current playlist file name; relative to smmDirectory, without
   // extension (suitable for user display). null if no playlist is
   // current.

   public static String playlistAbsPath()
   // return current playlist file abs path
   {
      return utils.smmDirectory + "/" + utils.playlistBasename + ".m3u";
   }

   public static String lastFileAbsPath(String basename)
   {
      // Return absolute path of file containing last song played
      return utils.smmDirectory + "/" + basename + ".last";
   }

   public static PendingIntent activityIntent;
   public static Intent        showDownloadLogIntent;
   public static Intent        showErrorLogIntent;
   // For notifications.

   public static Context mainActivity;

   ////////// methods

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

   final public static String logFileExt = ".txt";

   final public static String errorLogFileBaseName = "error_log";

   public static String errorLogFileName()
   {
      return utils.smmDirectory + "/" + errorLogFileBaseName + logFileExt;
   }

   public static void log(Context context, LogLevel level, String msg, String logFileBaseName)
   {
      final SimpleDateFormat fmt         = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss : ", Locale.US);
      final long             time        = System.currentTimeMillis(); // local time zone
      final String           timeStamp   = fmt.format(time);
      String                 levelImg    = level.image();
      String                 logFileName = utils.smmDirectory + "/" + logFileBaseName + logFileExt;
      File                   logFile     = new File(logFileName);

      if (logFile.exists() && time - logFile.lastModified() > 4 * utils.millisPerHour)
      {
         final String oldLogFileName = utils.smmDirectory + "/" + logFileBaseName + "_1" + logFileExt;
         File oldLogFile = new File(oldLogFileName);

         if (oldLogFile.exists()) oldLogFile.delete();

         logFile.renameTo(oldLogFile);
      }

      try
      {
         PrintWriter writer = new PrintWriter(new FileWriter(logFileName, true)); // append

         writer.println(timeStamp + levelImg + msg);
         writer.close();
      }
      catch (java.io.IOException e)
      {
         if (null != context)
            // null in unit tests and when called from Service
            utils.errorLog(context, "can't write log to " + logFileName, e);
      }
   }

   public static void debugLog(String msg)
   {
      if (BuildConfig.DEBUG)
         log(null, LogLevel.Debug, msg, errorLogFileBaseName);
   }

   public static void errorLog(Context context, String msg, Throwable e)
   {
      // programmer errors (possibly due to Android bugs :)
      log(context, LogLevel.Error, msg + e.toString(), errorLogFileBaseName);
      if (null != context)
         Toast.makeText(context, msg + e.toString(), Toast.LENGTH_LONG).show();
   }

   public static void errorLog(Context context, String msg)
   {
      // programmer errors (possibly due to Android bugs :)
      log (context, LogLevel.Error, msg, errorLogFileBaseName);

      // This can crash due to lack of resources; happens when run on new device.
      // Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
   }

   // The following write to the Android log, viewed with Android Studio

   // Must be shorter than 23 chars
   public static final String logTag =
      "stephes_music";
   //  1        10        20 |

   static void infoLog(Context context, String msg)
   {
      // helpful user messages, ie "could not play"; displayed for a short time.
      Log.i(logTag, msg);
      Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
   }

   public static void alertLog(Context context, String msg)
   {
      // Messages containing info user needs time to read; requires explicit dismissal.
      //
      // Cannot be called from a service
      Log.i(logTag, msg);
      new AlertDialog.Builder(context).setMessage(msg).setPositiveButton(R.string.Ok, null).show();
   }

   public static void alertLog(Context context, String msg, Throwable e)
   {
      // Messages containing info user needs time to read; requires explicit dismissal.
      //
      // Cannot be called from a service
      Log.e(logTag, msg);
      new AlertDialog.Builder(context).setMessage(msg + e.toString()).setPositiveButton(R.string.Ok, null).show();
   }

   static void verboseLog(String msg)
   {
      if (BuildConfig.DEBUG) Log.v(logTag, msg);
   }

}
