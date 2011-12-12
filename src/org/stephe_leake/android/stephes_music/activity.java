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
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.os.SystemClock;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.TextView;
import java.io.FilenameFilter;
import java.io.File;
import java.lang.Class;
import java.lang.Integer;

public class activity extends android.app.Activity
{
   // constants
   private static final int maxProgress     = 1000;
   private static final int DIALOG_PLAYLIST = 1;
   private static final int MENU_QUIT       = 0;

   // Main UI members

   private TextView    playlistTitle;
   private TextView    artistTitle;
   private TextView    albumTitle;
   private TextView    songTitle;
   private TextView    currentTime;
   private TextView    totalTime;
   private ImageButton playPauseButton;
   private SeekBar     progressBar;

   // Cached values
   private int    trackDuration = 0; // track duration in milliseconds
   private String playlistDir;

   private AlertDialog playlistDialog;

   ////////// UI listeners

   private ImageButton.OnClickListener Prev_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_PREVIOUS));
         }
      };

   private ImageButton.OnClickListener Play_Pause_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_TOGGLEPAUSE));
         }
      };

   private ImageButton.OnClickListener Next_Listener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_NEXT));
         }
      };

    private OnSeekBarChangeListener Progress_Listener = new OnSeekBarChangeListener()
       {
          long lastUserEventTime = 0;

          public void onStartTrackingTouch(SeekBar bar)
          {
             lastUserEventTime = SystemClock.elapsedRealtime();
          }

          public void onProgressChanged(SeekBar bar, int progress, boolean fromuser)
          {
             if (!fromuser) return;

             long now = SystemClock.elapsedRealtime();
             if ((now - lastUserEventTime) > 250) // milliseconds
             {
                lastUserEventTime = now;
                sendBroadcast
                   (new Intent(utils.ACTION_SEEK).
                    putExtra("position", (trackDuration * progress / maxProgress)));
             }
          }
          public void onStopTrackingTouch(SeekBar bar) {}
       };

   private TextView.OnClickListener playlistListener = new TextView.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            showDialog(DIALOG_PLAYLIST);
         }
      };

   ////////// Broadcast reciever

   private BroadcastReceiver broadcastReceiver = new BroadcastReceiver()
      {
         // see utils.java constants for list of intents

         @Override public void onReceive(Context context, Intent intent)
         {
            final String action = intent.getAction();

            utils.debugLog("activity broadcastReceiver action: " + action);
            try
            {
               if (action.equals(utils.META_CHANGED))
               {
                  artistTitle.setText(intent.getStringExtra("artist"));
                  albumTitle.setText(intent.getStringExtra("album"));
                  songTitle.setText(intent.getStringExtra("track"));
                  trackDuration = intent.getIntExtra("duration", 0);

                  totalTime.setText(utils.makeTimeString(activity.this, trackDuration));
                  playlistTitle.setText(intent.getStringExtra("playlist"));
               }
               else if (action.equals(utils.PLAYSTATE_CHANGED))
               {
                  final boolean playing = intent.getBooleanExtra("playing", false);
                  final int currentPos = intent.getIntExtra("position", 0);

                  if (playing)
                  {
                     playPauseButton.setImageResource(android.R.drawable.ic_media_pause);
                  }
                  else
                  {
                     playPauseButton.setImageResource(android.R.drawable.ic_media_play);
                  }

                  currentTime.setText(utils.makeTimeString(activity.this, currentPos));

                  if (trackDuration != 0)
                  {
                     progressBar.setProgress((int)(maxProgress * (long)currentPos/trackDuration));
                  }
               }
               else
               {
               utils.errorLog (activity.this, "broadcastReceiver got unexpected intent: " + intent.toString());
               }
            }
            catch (RuntimeException e)
            {
               utils.errorLog(activity.this, "broadcastReceiver: " + e.toString());
            }
         }
      };

   ////////// Activity lifetime methods (in lifecycle order)

   @Override public void onCreate(Bundle savedInstanceState)
   {
      // FIXME: get scale from preferences
      final float scale = 1.3f;

      final Intent intent = getIntent();

      try
      {
         super.onCreate(savedInstanceState);

         setContentView(R.layout.main);

         startService
            (new Intent().setComponent(new ComponentName (this, utils.serviceClassName)));

         // Set up displays, top to bottom left to right

         artistTitle = (TextView) findViewById(R.id.artistTitle);
         albumTitle  = (TextView) findViewById(R.id.albumTitle);
         songTitle   = (TextView) findViewById(R.id.songTitle);

         artistTitle.setTextSize(scale * artistTitle.getTextSize());
         albumTitle.setTextSize(scale * albumTitle.getTextSize());
         songTitle.setTextSize(scale * songTitle.getTextSize());

         ((ImageButton)findViewById(R.id.prev)).setOnClickListener(Prev_Listener);

         playPauseButton = (ImageButton)findViewById(R.id.play_pause);
         playPauseButton.setOnClickListener(Play_Pause_Listener);
         playPauseButton.requestFocus();

         ((ImageButton)findViewById(R.id.next)).setOnClickListener(Next_Listener);

         currentTime = (TextView)findViewById(R.id.currenttime);
         totalTime   = (TextView)findViewById(R.id.totaltime);

         progressBar = (SeekBar) findViewById(android.R.id.progress);
         progressBar.setOnSeekBarChangeListener(Progress_Listener);
         progressBar.setMax(maxProgress);

         playlistTitle = (TextView) findViewById(R.id.playlistTitle);
         playlistTitle.setTextSize(playlistTitle.getTextSize()); // not scaled to save screen space
         playlistTitle.setOnClickListener(playlistListener);

         // FIXME: get playlist dir from preferences
         playlistDir = "/sdcard/Audio";

         if (intent.getAction() == null || // destroyed/restored (ie for screen rotate)
             intent.getAction().equals(Intent.ACTION_MAIN)) // launched directly by user
         {
            // get current server state
            sendBroadcast (new Intent(utils.ACTION_UPDATE_DISPLAY));
         }
         else
         {
            utils.errorLog(this, "onCreate got unexpected intent: " + intent.toString());
         }
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "onCreate: That does not compute " + e.toString());
         finish();
      }
   }

   @Override protected void onResume()
   {
      super.onResume();
      try
      {
         IntentFilter f = new IntentFilter();
         f.addAction(utils.META_CHANGED);
         f.addAction(utils.PLAYSTATE_CHANGED);
         registerReceiver(broadcastReceiver, f);
         sendBroadcast(new Intent(utils.ACTION_UPDATE_DISPLAY));
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "onResume: " + e.toString());
      }
   }

   @Override protected void onPause()
   {
      super.onPause();
      unregisterReceiver(broadcastReceiver);
   }

   ////////// playlist dialog

   private class FileExtFilter implements FilenameFilter
   {
      String extension;

      FileExtFilter(String ext) {extension = ext;};

      @Override public boolean accept(File dir, String Filename)
      {
         return Filename.endsWith(extension);
      }
   }

   @Override protected Dialog onCreateDialog(int id)
   {
      switch (id)
      {
        case DIALOG_PLAYLIST:
           {
              try
              {
                 final File          playlistDir    = new File (this.playlistDir);
                 final FileExtFilter playlistFilter = new FileExtFilter (".m3u");
                 final String[] playlists           = playlistDir.list (playlistFilter);

                 playlistDialog = new AlertDialog.Builder(this)
                    .setTitle(R.string.dialog_playlist)
                    .setItems
                    (playlists,
                     new DialogInterface.OnClickListener()
                     {
                        public void onClick(DialogInterface dialog, int which)
                        {
                           try
                           {
                              final android.widget.ListView listView = playlistDialog.getListView();
                              final String filename = (String)listView.getAdapter().getItem(which);
                              sendBroadcast
                                 (new Intent (utils.ACTION_PLAYLIST).
                                  putExtra("playlist", playlistDir.getAbsolutePath() + "/" + filename));
                           }
                           catch (Exception e)
                           {
                              utils.errorLog(activity.this, "playlist dialog onClick: " + e.toString());
                           }
                        };
                     }
                     ).create();

                 return playlistDialog;
              }
              catch (Exception e)
              {
                 utils.errorLog(this, "create playlist dialog " + e.toString());
                 return null;
              }
           }
        default:
           utils.errorLog(this, "unknown dialog id " + id);
           return null;
        }
    }

   ////////// Menu

   @Override public boolean onCreateOptionsMenu(Menu menu)
   {
      super.onCreateOptionsMenu(menu);
      menu.add(0, MENU_QUIT, 0, R.string.menu_quit);
      return true; // display menu
   }

   @Override public boolean onOptionsItemSelected(MenuItem item)
   {
      switch (item.getItemId())
      {
      case MENU_QUIT:
         stopService
            (new Intent().setComponent(new ComponentName (this, utils.serviceClassName)));

         finish();
         break;

      default:
         utils.errorLog
            (this, "PlayerActivity.onOptionsItemSelected: unknown MenuItemId " + item.getItemId());
      }
      return false; // continue menu processing
   }
}
