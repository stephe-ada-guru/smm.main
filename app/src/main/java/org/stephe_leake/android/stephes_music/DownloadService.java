//  Abstract :
//
//  Provides background download from smm server.
//
//  Copyright (C) 2016 - 2019, 2021 Stephen Leake.  All Rights Reserved.
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

import android.app.PendingIntent;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.media.MediaScannerConnection;
import android.os.IBinder;
import android.preference.PreferenceManager;

import org.apache.commons.io.FilenameUtils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class DownloadService extends Service
{
   private DownloadNotif notif;

   ////////// private methods (alphabetical order)

   private int countSongsRemaining(String category, File playlistFile)
   throws IOException
   {
      // Duplicate the part of restoreState that gets playlistPos
      final String smmFileName = utils.smmDirectory + "/" + category + ".last";

      BufferedReader in        = new BufferedReader (new FileReader (playlistFile));
      String         line      = in.readLine();
      int            songCount = 0;
      int            startAt   = 0;

      String currentFile = null;

      if (new File(smmFileName).exists())
         try
         {
            BufferedReader reader = new BufferedReader(new FileReader(smmFileName));

            currentFile = reader.readLine();
            reader.close();
         }
         catch (IOException ignored) {}

      while (line != null)
      {
         if (new File(utils.smmDirectory, line).canRead())
         {
            if (line.equals(currentFile))
               startAt = songCount;
            songCount++;
         }
         line = in.readLine();
      }

      in.close();

      return songCount - startAt - 1;
   }

   private void download (Context context,
                          String playlistAbsName,
                          MediaScannerConnection mediaScanner,
                          DownloadNotif notif)
   {
      Resources         res     = getResources();
      SharedPreferences prefs   = PreferenceManager.getDefaultSharedPreferences(this);
      String songCountMaxStr    = prefs.getString
        (res.getString(R.string.song_count_max_key),
         res.getString(R.string.song_count_max_default));
      String newSongFractionStr    = prefs.getString
        (res.getString(R.string.new_song_fraction_key),
         res.getString(R.string.new_song_fraction_default));
      String overSelectRatioStr = prefs.getString
        (res.getString(R.string.over_select_ratio_key),
         res.getString(R.string.over_select_ratio_default));
      String songCountThreshStr = prefs.getString
        (res.getString(R.string.song_count_threshold_key),
         res.getString(R.string.song_count_threshold_default));

      String      serverIP        = prefs.getString (res.getString(R.string.server_IP_key), null);
      File        playlistFile    = new File(playlistAbsName);
      File        playlistDirFile = new File(FilenameUtils.getPath(playlistFile.getPath()));
      String      category        = FilenameUtils.getBaseName(playlistAbsName);
      StatusCount status          = new StatusCount();

      if (serverIP == null || serverIP.equals(""))
      {
         notif.Error("Server IP preference not set");
         return;
      }

      // File.exists throws IOException ENOENT if the directory does not exist!
      playlistDirFile.mkdirs();

      try
      {
         int songsRemaining  = playlistFile.exists() ? countSongsRemaining(category, playlistFile) : 0;
         int songCountMax    = Integer.parseInt(songCountMaxStr);
         int songCountThresh = Integer.parseInt(songCountThreshStr);
         float overSelectRatio = Float.parseFloat(overSelectRatioStr);

         if (songsRemaining < songCountMax - songCountThresh)
         {
            StatusStrings newSongs;
            int   songCount         = songCountMax - songsRemaining;
            float newSongCountFloat = songCount * Float.parseFloat(newSongFractionStr);
            int   newSongCount      = (int) newSongCountFloat;

            if (playlistFile.exists())
            {
               DownloadUtils.cleanPlaylist(context, category, playlistDirFile.getAbsolutePath(), utils.smmDirectory);

               if (utils.playlistAbsPath().equals(playlistAbsName))
               {
                  // Restart playlist to show new song position, count
                  sendBroadcast
                    (new Intent (utils.ACTION_PLAY_COMMAND)
                       .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PLAYLIST)
                       .putExtra(utils.EXTRA_COMMAND_PLAYLIST, playlistAbsName)
                       .putExtra(utils.EXTRA_COMMAND_STATE, PlayState.Paused.toInt()));
               }

               status.status = DownloadUtils.sendNotes(context, serverIP, category, utils.smmDirectory);
               if (status.status != ProcessStatus.Success)
                  return;
            }
            else
            {
               new File(playlistDirFile, category).mkdir();
               playlistFile.createNewFile();
            }

            newSongs =
              DownloadUtils.getNewSongsList(context, serverIP, category, songCount, newSongCount, overSelectRatio, -1);

            if (newSongs.status != ProcessStatus.Success)
            {
               notif.Error("get song list from server failed");
               return;
            }

            status = DownloadUtils.getSongs
              (context, serverIP, newSongs.strings, category, playlistDirFile.getAbsolutePath(), mediaScanner, notif);

            if (status.status != ProcessStatus.Success)
            {
               return;
            }

            if (utils.playlistAbsPath().equals(playlistAbsName))
            {
               // Restart playlist to show new song position, count
               sendBroadcast
                 (new Intent (utils.ACTION_PLAY_COMMAND)
                    .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PLAYLIST)
                    .putExtra(utils.EXTRA_COMMAND_PLAYLIST, playlistAbsName)
                    .putExtra(utils.EXTRA_COMMAND_STATE, PlayState.Paused.toInt()));
            }

            // cleanSongs after download new, to minimize metadata download
            DownloadUtils.cleanSongs(context, category, playlistDirFile.getAbsolutePath(), utils.smmDirectory);

            notif.Done("");
            DownloadUtils.log(context, LogLevel.Info, category + ": update done\n\n");

         }
         else
         {
            notif.Done("no downloaded needed");
            DownloadUtils.log(context, LogLevel.Info, category + ": no update needed\n\n");
         }
      }
      catch (IOException e)
      {
         // something is screwed up
         notif.Error("error: " + e.toString());
      }
   }

   class DownloadRun implements Runnable
   {
      private final String playlist;
      private final Context context;
      private final DownloadNotif notif;

      DownloadRun(Context context, DownloadNotif notif, String playlist)
      {
         this.context = context;
         this.notif = notif;
         this.playlist = playlist;
      }

      @Override public void run()
      {
         MediaScannerConnection mediaScanner   = new MediaScannerConnection(context, null);

         try
         {
            mediaScanner.connect();
            notif.setName(FilenameUtils.getBaseName(playlist));
            download(context, playlist, mediaScanner, notif);
         }
         finally
         {
            mediaScanner.disconnect();
         }
      }
   }

   private final BroadcastReceiver broadcastReceiverCommand = new BroadcastReceiver()
   {
      // Intent filter set for utils.ACTION_DOWNLOAD_COMMAND
      @Override public void onReceive(Context context, Intent intent)
      {
         final int command = intent.getIntExtra(utils.EXTRA_COMMAND, -1);

         switch (command)
         {
         case utils.COMMAND_CANCEL_DOWNLOAD:
            stopSelf();

         default:
         // just ignore.

         }
      }
   };

   ////////// service lifetime methods
   @Override public IBinder onBind(Intent intent)
   {
      return null;
   }

   @Override public void onCreate()
   {
      super.onCreate();

      IntentFilter filter = new IntentFilter();
      filter.addAction(utils.ACTION_DOWNLOAD_COMMAND);
      registerReceiver(broadcastReceiverCommand, filter);

      notif = new DownloadNotif
        (this,
         PendingIntent.getActivity
           (this.getApplicationContext(),
            utils.showDownloadLogIntentId,
            utils.showDownloadLogIntent,
            0),

         PendingIntent.getBroadcast
           (this.getApplicationContext(),
            utils.cancelDownloadIntentId,
            utils.cancelDownloadIntent,
            0));

      startForeground (utils.notif_download_id, notif.getNotif());
   }

   @Override public void onDestroy()
   {
      notif.Cancel();
      unregisterReceiver(broadcastReceiverCommand);
      super.onDestroy();
   }

   @Override public int onStartCommand(Intent intent, int flags, int startId)
   {
      if (intent == null)
      {
         // intent is null if the service is restarted by Android
         // after a crash.
         return START_NOT_STICKY;
      }
      else if (intent.getAction().equals(utils.ACTION_DOWNLOAD_COMMAND))
      {
         try
         {
            Resources         res   = getResources();
            SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);

            DownloadUtils.prefLogLevel = LogLevel.valueOf
              (prefs.getString(res.getString(R.string.log_level_key), LogLevel.Info.toString()));

            final String intentPlaylist = intent.getStringExtra(utils.EXTRA_COMMAND_PLAYLIST);

            DownloadRun runner = new DownloadRun (this, notif, intentPlaylist);
            new Thread(runner).start();

            return START_NOT_STICKY;
         }
         catch (Exception e)
         {
            utils.errorLog(this, "DownloadService::onCreate: ", e);
            return START_NOT_STICKY;
         }
      }
      else
      {
         utils.errorLog(null, "onStartCommand got bad intent: " + intent);
         return START_NOT_STICKY;
      }
   }
}
