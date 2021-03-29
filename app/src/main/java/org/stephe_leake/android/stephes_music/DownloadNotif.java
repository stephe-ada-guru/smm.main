//  Abstract :
//
//  Manage download notification.
//
//  Copyright (C) 2021 Stephen Leake. All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or
//  modify it under terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 3, or (at
//  your option) any later version. This program is distributed in the
//  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
//  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
//  PURPOSE. See the GNU General Public License for more details. You
//  should have received a copy of the GNU General Public License
//  distributed with this program; see file COPYING. If not, write to
//  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
//  MA 02110-1335, USA.

package org.stephe_leake.android.stephes_music;

import android.content.Context;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;

import org.jetbrains.annotations.NotNull;

public class DownloadNotif
{
   private static final String channelId = "Stephe's Music download service";

   private final Context context;
   private Notification notif;

   private final PendingIntent showLogPendingIntent;

   private String playlistName = "";
   private String statusText = "";
   private String contentText = "...";
   private Integer maxSongs = 0;
   private Integer currentSongs = 0;

   DownloadNotif(@NotNull Context context, PendingIntent showLogPendingIntent)
   {
      this.context = context;
      this.showLogPendingIntent = showLogPendingIntent;

      NotificationChannel channel = new NotificationChannel
            (channelId, "Stephe's Music download channel", NotificationManager.IMPORTANCE_LOW);
      channel.setLockscreenVisibility(Notification.VISIBILITY_PUBLIC);

      NotificationManager notificationManager = (NotificationManager)
        context.getSystemService(Context.NOTIFICATION_SERVICE);
      notificationManager.createNotificationChannel(channel);

      update();
   }

   Notification getNotif()
   {
      return notif;
   }

   void setName(String playlistName)
   {
      this.playlistName = playlistName;
   }

   void update()
   {
      NotificationManager notifManager = (NotificationManager)
        context.getSystemService(Context.NOTIFICATION_SERVICE);

      notif = new Notification.Builder(context, channelId)
        .setAutoCancel(true)
        .setContentIntent(showLogPendingIntent)
        .setContentTitle("Downloading " + playlistName + statusText)
        .setContentText(contentText)
        .setOngoing(true)
        .setProgress(maxSongs, currentSongs, maxSongs==0)
        .setSmallIcon(R.drawable.download_icon) // shown in status bar
        .build();

      notifManager.notify(utils.notif_download_id, notif);
   }

   void Done(String msg)
   {
      statusText = " done";
      contentText = msg;
      update();
   }

   void Error(String msg)
   {
      statusText = " done";
      contentText = msg;
      update();
   }

   void Update(Integer max, Integer current)
   {
      maxSongs = max;
      currentSongs = current;
      update();
   }

   void Cancel()
   {
      NotificationManager notifManager = (NotificationManager)
        context.getSystemService(Context.NOTIFICATION_SERVICE);

      notifManager.cancel(utils.notif_download_id);
      showLogPendingIntent.cancel();
   }
}
