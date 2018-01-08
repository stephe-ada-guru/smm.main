//  Abstract :
//
//  Provides background download from smm server.
//
//  Copyright (C) 2016 - 2018 Stephen Leake.  All Rights Reserved.
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
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.content.res.Resources;
import android.media.MediaScannerConnection;
import android.net.Uri.Builder;
import android.net.Uri;
import android.preference.PreferenceManager;
import android.preference.PreferenceManager;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.Integer;
import java.net.URISyntaxException;
import java.util.Calendar;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.GregorianCalendar;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.Timer;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;

public class DownloadService extends IntentService
{
   private static PendingIntent showLogPendingIntent;

   private Timer delayTimer;

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
      String newSongCountStr    = prefs.getString
         (res.getString(R.string.new_song_count_key),
          res.getString(R.string.new_song_count_default));
      String songCountThreshStr = prefs.getString
         (res.getString(R.string.song_count_threshold_key),
          res.getString(R.string.song_count_threshold_default));

      String      serverIP        = prefs.getString (res.getString(R.string.server_IP_key), null);
      File        playlistFile    = new File(playlistAbsName);
      File        playlistDirFile = new File(FilenameUtils.getPath(playlistFile.getPath()));
      String      category        = FilenameUtils.getBaseName(playlistAbsName);
      StatusCount status          = new StatusCount();

      if (serverIP == null)
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

         if (songsRemaining < songCountMax - songCountThresh)
         {
            StatusStrings newSongs;
            int           songCount         = songCountMax - songsRemaining;
            Float         newSongCountFloat = songCount * Float.valueOf(newSongCountStr);
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

            newSongs = DownloadUtils.getNewSongsList(context, serverIP, category, songCount, newSongCount, -1);

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
         Notification notif = new Notification.Builder(this)
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

         // Not in constructor, because showDownloadLogIntent can change
         // if user changes preference.
         //
         // FIXME: change to fileprovider
         // showLogPendingIntent = PendingIntent.getActivity
         //    (this.getApplicationContext(),
         //     utils.showDownloadLogIntentId,
         //     utils.showDownloadLogIntent,
         //     0);

         {
            Resources         res   = getResources();
            SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);

            DownloadUtils.prefLogLevel = LogLevel.valueOf
               (prefs.getString(res.getString(R.string.log_level_key), LogLevel.Info.toString()));
         }

         mediaScanner.connect();

         if (null == intentPlaylist)
         {
            Resources         res       = getResources();
            SharedPreferences prefs     = PreferenceManager.getDefaultSharedPreferences(this);
            Set<String>       playlists = prefs.getStringSet
               (res.getString(R.string.auto_download_playlists_key),
                new LinkedHashSet<String>());

            msg = "Downloading " + playlists.size() + " playlists";

            notifyDownload(msg, "...");

            for (String playlist : playlists)
            {
               status = download(this, utils.smmDirectory + "/" + playlist + ".m3u", mediaScanner);
               totalCount += status.count;
               if (status.status == ProcessStatus.Retry)
               {
                  // Apparently we cannot use a timer twice.
                  delayTimer = new Timer();
                  delayTimer.schedule(utils.downloadTimerTask, 10 * utils.millisPerMinute);
                  break;
               }
            }
         }
         else
         {
            msg = "Downloading " + FilenameUtils.getBaseName(intentPlaylist);

            notifyDownload(msg, "...");

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

         mediaScanner.disconnect();
      }
      catch (Exception e)
      {
         utils.errorLog(this, "DownloadService::onHandleIntent: ", e);
      }
   }
}
