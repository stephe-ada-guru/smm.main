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

import android.app.IntentService;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.media.MediaScannerConnection;
import android.preference.PreferenceManager;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.Integer;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.Timer;
import org.apache.commons.io.FilenameUtils;

public class DownloadService extends IntentService
{
   private static final String channelId   = "Stephe's Music download service";

   private static PendingIntent showLogPendingIntent;

   private Timer delayTimer;
   private Notification notif;

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
         catch (IOException e) {}

      while (line != null)
      {
         if (new File(utils.smmDirectory, line).canRead())
            {
               if (currentFile != null && line.equals(currentFile))
                  startAt = songCount;
               songCount++;
            }
         line = in.readLine();
      }

      in.close();

      return songCount - startAt - 1;
   }

   private StatusCount download(Context context, String playlistAbsName, MediaScannerConnection mediaScanner)
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

      if (serverIP == null || serverIP == "")
      {
         notifyDownload("Download error", "set Server IP preference");
         status.status = ProcessStatus.Fatal;
         return status;
      }

      // File.exists throws IOException ENOENT if the directory does not exist!
      playlistDirFile.mkdirs();

      try
      {
         int songsRemaining  = playlistFile.exists() ? countSongsRemaining(category, playlistFile) : 0;
         int songCountMax    = Integer.valueOf(songCountMaxStr);
         int songCountThresh = Integer.valueOf(songCountThreshStr);
         float overSelectRatio = Float.valueOf(overSelectRatioStr);

         if (songsRemaining < songCountMax - songCountThresh)
         {
            StatusStrings newSongs;
            int           songCount         = songCountMax - songsRemaining;
            Float         newSongCountFloat = songCount * Float.valueOf(newSongFractionStr);
            int           newSongCount      = newSongCountFloat.intValue();

            if (playlistFile.exists())
            {
               DownloadUtils.cleanPlaylist(context, category, playlistDirFile.getAbsolutePath(), utils.smmDirectory);

               if (utils.playlistAbsPath().equals(playlistAbsName))
               {
                  // Restart playlist to show new song position, count
                  sendBroadcast
                     (new Intent (utils.ACTION_COMMAND)
                      .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PLAYLIST)
                      .putExtra(utils.EXTRA_COMMAND_PLAYLIST, playlistAbsName)
                      .putExtra(utils.EXTRA_COMMAND_STATE, PlayState.Paused.toInt()));
               }

               status.status = DownloadUtils.sendNotes(context, serverIP, category, utils.smmDirectory);
               if (status.status != ProcessStatus.Success)
                  return status;
            }
            else
            {
               new File(playlistDirFile, category).mkdir();
               playlistFile.createNewFile();
            }

            newSongs =
              DownloadUtils.getNewSongsList(context, serverIP, category, songCount, newSongCount, overSelectRatio, -1);

            if (newSongs.strings.length == 0)
            {
               status.status = newSongs.status;
               status.count = 0;
               return status;
            }

            status = DownloadUtils.getSongs
               (context, serverIP, newSongs.strings, category, playlistDirFile.getAbsolutePath(), mediaScanner);

            if (status.status != ProcessStatus.Success)
               return status;

            if (utils.playlistAbsPath().equals(playlistAbsName))
            {
               // Restart playlist to show new song position, count
               sendBroadcast
                  (new Intent (utils.ACTION_COMMAND)
                   .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PLAYLIST)
                   .putExtra(utils.EXTRA_COMMAND_PLAYLIST, playlistAbsName)
                   .putExtra(utils.EXTRA_COMMAND_STATE, PlayState.Paused.toInt()));
            }

            // cleanSongs after download new, to minimize metadata download
            DownloadUtils.cleanSongs(context, category, playlistDirFile.getAbsolutePath(), utils.smmDirectory);

            DownloadUtils.log(context, LogLevel.Info, category + ": update done\n\n");

         }
         else
            DownloadUtils.log(context, LogLevel.Info, category + ": no update needed\n\n");
      }
      catch (IOException e)
      {
         // something is screwed up
         notifyDownload("Download error", e.toString());
         status.status = ProcessStatus.Fatal;
      }
      return status;
   }

   private void notifyDownload(String title, String msg)
   {
      try
      {
         NotificationChannel channel = new NotificationChannel
           (channelId, "Stephe's Music download channel", NotificationManager.IMPORTANCE_MIN);
         channel.setLockscreenVisibility(Notification.VISIBILITY_SECRET);

         NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
         notificationManager.createNotificationChannel(channel);

         notif = new Notification.Builder(this, channelId)
            .setAutoCancel(false)
            .setContentTitle(title)
            .setContentText(msg)
            .setContentIntent(showLogPendingIntent)
            .setSmallIcon(R.drawable.download_icon) // shown in status bar
            .build();

         try
         {
            NotificationManager notifManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
            notifManager.notify(null, utils.notif_download_id, notif);
         }
         catch (RuntimeException e)
         {
            utils.errorLog(this, "notify " + e.toString());
         }
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "notify build " + e.toString());
      }
   }

   ////////// service lifetime methods

   public DownloadService()
   {
      super("Stephe's Music Download service");
   }

   @Override
   public void onHandleIntent(Intent intent)
   {
      try
      {
         final String           intentPlaylist = intent.getStringExtra(utils.EXTRA_COMMAND_PLAYLIST);
         MediaScannerConnection mediaScanner   = new MediaScannerConnection(this, null);
         String                 msg            = "";
         StatusCount            status         = new StatusCount();
         Integer                totalCount     = 0;

         showLogPendingIntent = PendingIntent.getActivity
           (this.getApplicationContext(),
            utils.showDownloadLogIntentId,
            utils.showDownloadLogIntent,
            0);

         {
            Resources         res   = getResources();
            SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);

            DownloadUtils.prefLogLevel = LogLevel.valueOf
              (prefs.getString(res.getString(R.string.log_level_key), LogLevel.Info.toString()));
         }

         try
         {
            mediaScanner.connect();

            {
               msg = "Downloading " + FilenameUtils.getBaseName(intentPlaylist);

               notifyDownload(msg, "...");

               startForeground (1, notif);

               status = download(this, intentPlaylist, mediaScanner);

               totalCount += status.count;
            }

            switch (status.status)
            {
            case Success:
               notifyDownload(msg, "downloaded " + totalCount.toString() + " songs.");
               break;

            case Retry:
               notifyDownload(msg, "delayed ...");
               break;

            case Fatal:
               notifyDownload(msg, "error");
               break;
            }
         }
         finally
         {
            mediaScanner.disconnect();
         }
      }
      catch (Exception e)
      {
         utils.errorLog(this, "DownloadService::onHandleIntent: ", e);
      }
   }
}
