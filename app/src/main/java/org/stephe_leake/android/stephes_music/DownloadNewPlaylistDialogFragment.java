//  Abstract :
//
//  Dialog to enter a new playlist category to download
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
import android.widget.ListView;
import android.widget.EditText;
import android.view.inputmethod.InputMethodManager;
import android.preference.PreferenceManager;
import android.content.Context;
import android.content.DialogInterface;
import android.app.Activity;
import android.content.Intent;

public class DownloadNewPlaylistDialogFragment extends DialogFragment
{
   @Override
   public Dialog onCreateDialog(Bundle saved)
   {
      final Activity activity = this.getActivity();

      try
      {
         Resources         res   = getResources();
         SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);
         final File playlistDir  = new File
            (prefs.getString
             (res.getString(R.string.playlist_directory_key),
              res.getString(R.string.playlist_directory_default)));

         View view = getActivity().getLayoutInflater().inflate(R.layout.dialog_download_new, null);
         final EditText textView = (EditText)view.findViewById(R.id.text_view);

         // Force display keboard for new name entry.
         InputMethodManager imm = (InputMethodManager)activity.getSystemService(Context.INPUT_METHOD_SERVICE);
         imm.showSoftInput(textView, InputMethodManager.SHOW_IMPLICIT);

         AlertDialog dialog = new AlertDialog.Builder(activity)
            .setTitle(R.string.dialog_download_new_playlist)
            .setView(view)
            .setPositiveButton(R.string.download, new DialogInterface.OnClickListener()
               {
                  public void onClick(DialogInterface dialog, int which)
                  {
                     String filename = null;

                     // User entered new playlist name in edit box
                     filename = textView.getText().toString() + ".m3u";

                     activity.startService
                        (new Intent (utils.ACTION_COMMAND, null, null, DownloadService.class)
                         .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_DOWNLOAD)
                         .putExtra(utils.EXTRA_COMMAND_PLAYLIST, playlistDir.getAbsolutePath() +
                                   "/" + filename));
                  };
               }
               ).create();

         return dialog;
      }
      catch (Exception e)
      {
         utils.alertLog(activity, "create download new playlist dialog failed ", e);
         return null;
      }
   }
}
