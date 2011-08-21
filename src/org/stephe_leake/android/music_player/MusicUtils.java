/*
 * Copyright (C) 2011 Stephen Leake
 * Copyright (C) 2008 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Copied from com.android.music in Android 2.3.3; modified to be in
// my package; deleted everything not needed by my package. Order of
// declarations is the same as in the original, so the diff is
// minimized.
// (ediff "c:/Projects/android/google/packages/apps/Music/src/com/android/music/MusicUtils.java" "MusicUtils.java")

package org.stephe_leake.android.music_player;

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
import android.os.SystemClock;
import android.text.format.Time;
import android.util.Log;
import android.widget.Toast;

import java.io.PrintWriter;
import java.util.HashMap;

public class MusicUtils {

   public static IMediaPlaybackService sService = null;

   // We need a map of context to servicebinder because we support
   // multiple activities sharing the same service; music player,
   // music picker.
   //
   // On the other hand, I'm not clear why each activity doesn't just
   // bind directly (Player already implements ServiceConnection)!
   private static HashMap<Context, ServiceBinder> sConnectionMap = new HashMap<Context, ServiceBinder>();

   public static class ServiceToken {
      ContextWrapper mWrappedContext;
      ServiceToken(ContextWrapper context) {
         mWrappedContext = context;
      }
   }

   public static ServiceToken bindToService(Activity context) {
      return bindToService(context, null);
   }

   public static ServiceToken bindToService(Activity context, ServiceConnection callback) {
      Activity realActivity = context.getParent();
      if (realActivity == null) {
         realActivity = context;
      }
      ContextWrapper cw = new ContextWrapper(realActivity);
      cw.startService(new Intent(cw, Stephes_Music_Service.class));
      ServiceBinder sb = new ServiceBinder(callback);
      if (cw.bindService((new Intent()).setClass(cw, Stephes_Music_Service.class), sb, 0))
      {
         // The service is _not_ connected at this point, and won't be
         // until the activity calling bindToService is fully started.
         // Thus if the activity cannot use the service from onCreate;
         // instead, it must implement ServiceConnection, and provide
         // a callback that handles onServiceConnected; it can then
         // interact with the service in that callback.
         sConnectionMap.put(cw, sb);
         return new ServiceToken(cw);
      }
      // Can't use Error_Log (or debugLog) here, because that is only
      // dumped by the service.
      Log.e(Log_App_Name, "Failed to bind to service");
      return null;
   }

   public static void unbindFromService(ServiceToken token) {
      if (token == null) {
         Log.e(Log_App_Name, "Trying to unbind with null token");
         return;
      }
      ContextWrapper cw = token.mWrappedContext;
      ServiceBinder sb = sConnectionMap.remove(cw);
      if (sb == null) {
         Log.e(Log_App_Name, "Trying to unbind for unknown Context");
         return;
      }
      cw.unbindService(sb);
      if (sConnectionMap.isEmpty()) {
         // presumably there is nobody interested in the service at this point,
         // so don't hang on to the ServiceConnection
         sService = null;
      }
   }

   private static class ServiceBinder implements ServiceConnection {
      ServiceConnection mCallback;
      ServiceBinder(ServiceConnection callback) {
         mCallback = callback;
      }

      public void onServiceConnected(ComponentName className, android.os.IBinder service) {
         sService = IMediaPlaybackService.Stub.asInterface(service);
         if (mCallback != null) {
            mCallback.onServiceConnected(className, service);
         }
      }

      public void onServiceDisconnected(ComponentName className) {
         if (mCallback != null) {
            mCallback.onServiceDisconnected(className);
         }
         sService = null;
      }
   }

   public static void addToCurrentPlaylist(Context context, long [] list) {
      try {
         sService.enqueue(list, Stephes_Music_Service.LAST);
      } catch (RemoteException ex) {
      }
   }

   public static Cursor query(Context context, Uri uri, String[] projection,
                              String selection, String[] selectionArgs, String sortOrder, int limit) {
      try {
         ContentResolver resolver = context.getContentResolver();
         if (resolver == null) {
            return null;
         }
         if (limit > 0) {
            uri = uri.buildUpon().appendQueryParameter("limit", "" + limit).build();
         }
         return resolver.query(uri, projection, selection, selectionArgs, sortOrder);
      } catch (UnsupportedOperationException ex) {
         return null;
      }

   }

   public static Cursor query(Context context, Uri uri, String[] projection,
                              String selection, String[] selectionArgs, String sortOrder) {
      return query(context, uri, projection, selection, selectionArgs, sortOrder, 0);
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

   static void debugLog(Object o)
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

   //////////
   // my new stuff

   final public static String Log_App_Name = "stephes_music_player"; // for use in log messages

   static void Error_Log(Context context, String msg)
   {
      debugLog(msg);
      Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
   }

   static void Info_Log(Context context, String msg)
   {
      debugLog(msg);
      Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
   }

   public static String getAlbumName()
   {
      try
      {
         return sService.getAlbumName();
      } 
      catch (RemoteException ex) 
      { 
         return "";
      }
   }

   public static String getArtistName()
   {
      try
      {
         return sService.getArtistName();
      }
      catch (RemoteException ex)
      {
         return "";
      }
   }

   public static String getTrackName()
   {
      try
      {
         return sService.getTrackName();
      } 
      catch (RemoteException ex) 
      { 
         return "";
      }
   }

   // Start playing current playlist; noop if already playing
   public static void play()
   {
      // FIXME: replace by broadcast intent command.
      try
      {
         if (! sService.isPlaying())
         {
            // yes, next does what we want.
            sService.next();
         }
      } catch (RemoteException ex) { }
   }
}
