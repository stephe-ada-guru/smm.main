//  Abstract :
//
//  Provides background download from smm server.
//
//  Copyright (C) 2016 Stephen Leake.  All Rights Reserved.
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

import android.app.Notification;
import android.app.NotificationManager;
import android.app.IntentService;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Resources;
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
import java.util.GregorianCalendar;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Set;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.FileUtils;
import android.content.Context;
import android.media.MediaScannerConnection;

public class DownloadService extends IntentService
{
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
         if (new File(utils.playlistDirectory, line).canRead())
            {
               if (currentFile != null && line.equals(currentFile))
                  startAt = songCount;
               songCount++;
            }
         line = in.readLine();
      }

      in.close();

      return songCount - startAt;
   }

   private void download(String playlistAbsName, MediaScannerConnection mediaScanner)
   {
      Resources         res             = getResources();
      SharedPreferences prefs           = PreferenceManager.getDefaultSharedPreferences(this);
      String songCountMaxStr            = prefs.getString
         (res.getString(R.string.song_count_max_key),
          res.getString(R.string.song_count_max_default));
      String songCountThreshStr         = prefs.getString
         (res.getString(R.string.song_count_threshold_key),
          res.getString(R.string.song_count_threshold_default));
      String            serverIP        = prefs.getString (res.getString(R.string.server_IP_key), null);
      File              playlistFile    = new File(playlistAbsName);
      File              playlistDirFile = new File(FilenameUtils.getPath(playlistFile.getPath()));
      String            category        = FilenameUtils.getBaseName(playlistAbsName);

      if (serverIP == null)
      {
         notifyDownload("Error", "set Server IP preference");
         return;
      }

      // File.exists throws IOException ENOENT if the directory does not exist!
      playlistDirFile.mkdirs();

      try
      {
         int songsRemaining = playlistFile.exists() ? countSongsRemaining(category, playlistFile) : 0;
         int songCountMax    = Integer.valueOf(songCountMaxStr);
         int songCountThresh = Integer.valueOf(songCountThreshStr);

         if (songsRemaining < songCountMax - songCountThresh)
         {
            String[] newSongs;

            if (playlistFile.exists())
               DownloadUtils.firstPass(category, playlistDirFile.getAbsolutePath(), utils.smmDirectory);
            else
            {
               new File(playlistDirFile, category).mkdir();
               playlistFile.createNewFile();
            }

            newSongs = DownloadUtils.getNewSongsList(serverIP, category, songCountMax - songsRemaining, -1);
            DownloadUtils.getSongs(serverIP, newSongs, category, playlistDirFile.getAbsolutePath(), mediaScanner);
         }
      }
      catch (IOException e)
      {
         // something is screwed up
         notifyDownload("download error", e.toString());
      }
      catch (URISyntaxException e)
      {
         // song resource can't be encoded in a URI; not likely!
         utils.errorLog(this, "download: ", e);
      }
   }

   private void notifyDownload(String title, String msg)
   {
      try
      {
         Notification notif = new Notification.Builder(this)
            .setAutoCancel(false)
            .setContentTitle(title)
            .setStyle(new Notification.BigTextStyle().bigText(msg))
            .setContentIntent(utils.activityIntent)
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
      final String intentPlaylist = intent.getStringExtra(utils.EXTRA_COMMAND_PLAYLIST);
      MediaScannerConnection mediaScanner = new MediaScannerConnection(this, null);

      try
      {
         mediaScanner.connect();

         if (null == intentPlaylist)
         {
            Resources         res       = getResources();
            SharedPreferences prefs     = PreferenceManager.getDefaultSharedPreferences(this);
            Set<String>       playlists = prefs.getStringSet
               (res.getString(R.string.auto_download_playlists_key),
                new LinkedHashSet<String>());

            notifyDownload("Downloading " + playlists.size() + " playlists", "");

            for (String playlist : playlists)
               download(utils.playlistDirectory + "/" + playlist + ".m3u", mediaScanner);
         }
         else
         {
            notifyDownload("Downloading " + FilenameUtils.getBaseName(intentPlaylist), "");

            download(intentPlaylist, mediaScanner);
         }

         notifyDownload("Downloading done", "");
      }
      finally
      {
         mediaScanner.disconnect();
      }
   }
}
