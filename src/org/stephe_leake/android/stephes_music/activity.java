//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011, 2012 Stephen Leake.  All Rights Reserved.
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
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.preference.PreferenceManager;
import android.os.Bundle;
import android.os.SystemClock;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.ScrollView;
import android.widget.TextView;
import java.io.File;
import java.lang.Class;
import java.lang.Float;
import java.lang.Integer;

public class activity extends android.app.Activity
{
   // constants
   private static final int maxProgress      = 1000;
   private static final int DIALOG_PLAYLIST  = 1;
   private static final int MENU_QUIT        = 0;
   private static final int MENU_DUMP_LOG    = 1;
   private static final int MENU_SAVE_STATE  = 2;
   private static final int MENU_PREFERENCES = 3;

   private static final int RESULT_PREFERENCES = 1;

   // compatibility constants; these are not defined in API level 10,
   // but we need them for higher level devices.
   private static final int KEYCODE_MEDIA_PAUSE = 127;
   private static final int KEYCODE_MEDIA_PLAY  = 126;

   // Main UI members

   private TextView    artistTitle;
   private TextView    albumTitle;
   private TextView    songTitle;
   private TextView    currentTime;
   private TextView    totalTime;
   private ImageButton playPauseButton;
   private SeekBar     progressBar;
   private TextView    playlistTitle;

   // Cached values
   private int trackDuration = 0; // track duration in milliseconds
   private float defaultTextViewTextSize; // set in onCreate

   private AlertDialog playlistDialog;

   ////////// local utils

   private float getTextViewTextScale()
   {
      Resources         res   = getResources();
      SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
      String scale            = prefs.getString
             (res.getString(R.string.text_scale_key),
              res.getString(R.string.text_scale_default));
      try
      {
         return new Float(scale).floatValue();
      }
      catch (NumberFormatException e)
      {
         utils.errorLog(this, "invalid text_scale preference: " + scale);
         return 1.3f;
      }
   };

   ////////// UI listeners (alphabetical by listener name; some defined in main.xml)

   public void onClickNote(View v)
   {
      // FIXME: this is language dependent; need to derive new Button
      // view, store a standard note key string.
      sendBroadcast
         (new Intent
          (utils.ACTION_COMMAND)
          .putExtra("command", utils.COMMAND_NOTE)
          .putExtra("note", ((String)((Button)v).getText()).replace('\n', ' ')));
   };

   private ImageButton.OnClickListener nextListener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_NEXT));
         }
      };

   private TextView.OnClickListener playlistListener = new TextView.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            showDialog(DIALOG_PLAYLIST);
         }
      };

   private ImageButton.OnClickListener playPauseListener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_TOGGLEPAUSE));
         }
      };

   private ImageButton.OnClickListener prevListener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_PREVIOUS));
         }
      };

   private OnSeekBarChangeListener progressListener = new OnSeekBarChangeListener()
      {
         // The system generates events very fast; that leads to a
         // stuttering sound. So add some time hysteresis.

         private long lastTime = 0;

         public void onStartTrackingTouch(SeekBar bar)
         {
         }

         public void onProgressChanged(SeekBar bar, int progress, boolean fromuser)
         {
            if (!fromuser) return;

            final long currentTime = System.currentTimeMillis();

            if (currentTime > lastTime + 100) // 0.1 seconds
            {
               lastTime = currentTime;

               sendBroadcast
                  (new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_SEEK).
                   putExtra("position", (trackDuration * progress / maxProgress)));
            }
         }

         public void onStopTrackingTouch(SeekBar bar)
         {
         }
      };

   ////////// Broadcast reciever

   private BroadcastReceiver broadcastReceiver = new BroadcastReceiver()
      {
         // see utils.java constants for list of intents

         @Override public void onReceive(Context context, Intent intent)
         {
            final String action = intent.getAction();

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
               utils.errorLog(activity.this, "broadcastReceiver: ", e);
            }
         }
      };

   ////////// Activity lifetime methods (in lifecycle order)

   @Override public void onCreate(Bundle savedInstanceState)
   {
      PreferenceManager.setDefaultValues(this, R.xml.preferences, false);

      final float scale = getTextViewTextScale();

      final Intent intent = getIntent();

      try
      {
         super.onCreate(savedInstanceState);

         setContentView(R.layout.main);

         startService (new Intent(this, service.class));

         // Set up displays, top to bottom left to right

         artistTitle = utils.findTextViewById(this, R.id.artistTitle);
         albumTitle  = utils.findTextViewById(this, R.id.albumTitle);
         songTitle   = utils.findTextViewById(this, R.id.songTitle);

         defaultTextViewTextSize = artistTitle.getTextSize();
         artistTitle.setTextSize(scale * defaultTextViewTextSize);
         albumTitle.setTextSize(scale * defaultTextViewTextSize);
         songTitle.setTextSize(scale * defaultTextViewTextSize);

         // FIXME: set button text size from preference. find
         // notes_buttons_* linear layout(s), iterate over children

         ((ImageButton)findViewById(R.id.prev)).setOnClickListener(prevListener);

         playPauseButton = (ImageButton)findViewById(R.id.play_pause);
         playPauseButton.setOnClickListener(playPauseListener);
         playPauseButton.requestFocus();

         ((ImageButton)findViewById(R.id.next)).setOnClickListener(nextListener);

         currentTime = (TextView)findViewById(R.id.currenttime);
         totalTime   = (TextView)findViewById(R.id.totaltime);

         progressBar = (SeekBar) findViewById(android.R.id.progress);
         progressBar.setOnSeekBarChangeListener(progressListener);
         progressBar.setMax(maxProgress);

         playlistTitle = utils.findTextViewById(this, R.id.playlistTitle);
         playlistTitle.setTextSize(scale * defaultTextViewTextSize);
         playlistTitle.setOnClickListener(playlistListener);

         if (intent.getAction() == null || // destroyed/restored (ie for screen rotate)
             intent.getAction().equals(Intent.ACTION_MAIN)) // launched directly by user
         {
            // get current server state
            sendBroadcast (new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_UPDATE_DISPLAY));
         }
         else
         {
            utils.errorLog(this, "onCreate got unexpected intent: " + intent.toString());
         }
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "onCreate: That does not compute: " + e.getMessage(), e);
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
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_UPDATE_DISPLAY));
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "onResume: ", e);
      }
   }

   @Override protected void onPause()
   {
      super.onPause();
      unregisterReceiver(broadcastReceiver);
   }

   @Override protected void onDestroy()
   {
      super.onDestroy();
      utils.verboseLog ("activity onDestroy");

      // FIXME: need stopService here?
   }

   ////////// key handling

   @Override public boolean onKeyDown(int keyCode, KeyEvent event)
   {
      boolean handled = false; // don't terminate event processing; let MediaEventReceivers get it

      switch (keyCode)
      {
         // Alphabetical keycode order
      case KeyEvent.KEYCODE_MEDIA_NEXT:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_NEXT));
         handled = true; // terminate event processing; MediaEventReceivers won't get it
         break;

      case KEYCODE_MEDIA_PAUSE:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_PAUSE));
         handled = true;
         break;

      case KEYCODE_MEDIA_PLAY:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_PLAY));
         handled = true;
         break;

      case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_TOGGLEPAUSE));
         handled = true;
         break;

      case KeyEvent.KEYCODE_MEDIA_PREVIOUS:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_PREVIOUS));
         handled = true;
         break;

      case KeyEvent.KEYCODE_MEDIA_STOP:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_PAUSE));
         handled = true;
         break;

      default:
      }
      return handled;
   };


   ////////// playlist dialog

   @Override protected Dialog onCreateDialog(int id, Bundle args)
   {
      switch (id)
      {
      case DIALOG_PLAYLIST:
         {
            try
            {
               Resources res = getResources();
               SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);

               final File playlistDir = new File
                  (prefs.getString
                   (res.getString(R.string.playlist_directory_key),
                    res.getString(R.string.playlist_directory_default)));

               final FileExtFilter playlistFilter = new FileExtFilter(".m3u");
               final String[] playlists           = playlistDir.list(playlistFilter);

               if (playlists == null || playlists.length == 0)
               {
                  utils.infoLog(this, "no playlists found in " + playlistDir);
                  return null;
               }

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
                               (new Intent (utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_PLAYLIST).
                                putExtra("playlist", playlistDir.getAbsolutePath() + "/" + filename));
                         }
                         catch (Exception e)
                         {
                            utils.errorLog(activity.this, "playlist dialog onClick: ", e);
                         }
                      };
                   }
                   ).create();

               return playlistDialog;
            }
            catch (Exception e)
            {
               utils.errorLog(this, "create playlist dialog failed ", e);
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
      menu.add(0, MENU_DUMP_LOG, 0, R.string.menu_dump_log);
      menu.add(0, MENU_SAVE_STATE, 0, R.string.menu_save_state);
      menu.add(0, MENU_PREFERENCES, 0, R.string.menu_preferences);
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

      case MENU_DUMP_LOG:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_DUMP_LOG));
         break;

      case MENU_SAVE_STATE:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra("command", utils.COMMAND_SAVE_STATE));
         break;

      case MENU_PREFERENCES:
         startActivityForResult (new Intent(this, preferences.class), RESULT_PREFERENCES);
         break;

      default:
         utils.errorLog
            (this, "activity.onOptionsItemSelected: unknown MenuItemId " + item.getItemId());
      }
      return false; // continue menu processing
   }

   @Override protected void onActivityResult (int requestCode, int resultCode, Intent data)
   {
      switch(requestCode)
      {
      case RESULT_PREFERENCES:
         switch (resultCode)
         {
         case RESULT_CANCELED:
         case RESULT_OK:
            break;

         case utils.RESULT_TEXT_SCALE:
            {
               final float scale = getTextViewTextScale();

               artistTitle.setTextSize(scale * defaultTextViewTextSize);
               albumTitle.setTextSize(scale * defaultTextViewTextSize);
               songTitle.setTextSize(scale * defaultTextViewTextSize);
            }
            break;

         default:
            utils.errorLog
               (this, "activity.onActivityResult: unknown preferences resultCode " + resultCode);
            break;
         }
         break;

      default:
         utils.errorLog
            (this, "activity.onActivityResult: unknown requestCode " + requestCode);
      }
   }
}
