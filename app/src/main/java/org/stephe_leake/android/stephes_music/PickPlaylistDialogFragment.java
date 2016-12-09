//  Abstract :
//
//  Dialog for picking an available playlist to play
//
//  Copyright (C) 2016 Stephen Leake. All Rights Reserved.
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

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.os.Bundle;
import android.content.res.Resources;
import android.content.SharedPreferences;
import java.io.File;
import android.view.View;
import android.preference.PreferenceManager;
import android.app.Activity;
import android.content.DialogInterface;
import android.content.Intent;

public class PickPlaylistDialogFragment extends DialogFragment
{
   // Need an external reference for onClick
   private AlertDialog dialogPlayPlaylist;

   @Override
   public Dialog onCreateDialog(Bundle saved)
   {
      final Activity activity = this.getActivity();
      final int      command  = getArguments().getInt("command");

      try
      {
         Resources         res   = getResources();
         SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);
         final File playlistDir  = new File
            (prefs.getString
             (res.getString(R.string.playlist_directory_key),
              res.getString(R.string.playlist_directory_default)));

         final FileExtFilter playlistFilter = new FileExtFilter(".m3u");
         final String[] playlists           = playlistDir.list(playlistFilter);

         if (playlists == null || playlists.length == 0)
         {
            utils.alertLog(activity, "no playlists found in " + playlistDir);
            return null;
         }

         dialogPlayPlaylist = new AlertDialog.Builder(activity)
            .setTitle(R.string.dialog_play_playlist)
            .setItems
            (playlists,
             new DialogInterface.OnClickListener()
             {
                public void onClick(DialogInterface dialogInt, int which)
                {
                   try
                   {
                      final android.widget.ListView listView = dialogPlayPlaylist.getListView();
                      final String filename = (String)listView.getAdapter().getItem(which);

                      switch (command)
                      {
                      case utils.COMMAND_DOWNLOAD:
                         activity.startService
                            (new Intent (utils.ACTION_COMMAND, null, activity, DownloadService.class)
                             .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_DOWNLOAD)
                             .putExtra(utils.EXTRA_COMMAND_PLAYLIST, playlistDir.getAbsolutePath() + "/" + filename));
                         break;

                      case utils.COMMAND_PLAYLIST:
                         activity.sendBroadcast
                            (new Intent (utils.ACTION_COMMAND)
                             .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PLAYLIST)
                             .putExtra(utils.EXTRA_COMMAND_PLAYLIST, playlistDir.getAbsolutePath() + "/" + filename));
                      };

                   }
                   catch (Exception e)
                   {
                      utils.alertLog(activity, "pick playlist dialog onClick: ", e);
                   }
                };
             }
             ).create();

         return dialogPlayPlaylist;
      }
      catch (Exception e)
      {
         utils.alertLog(activity, "create pick playlist dialog failed ", e);
         return null;
      }
   }

}
