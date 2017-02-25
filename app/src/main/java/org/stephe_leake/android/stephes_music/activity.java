//  Abstract :
//
//  Provides User Interface to Stephe's Music Player.
//
//  Copyright (C) 2011 - 2013, 2015 - 2017 Stephen Leake.  All Rights Reserved.
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
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.net.Uri.Builder;
import android.net.Uri;
import android.os.Bundle;
import android.os.PowerManager.WakeLock;
import android.os.PowerManager;
import android.os.SystemClock;
import android.preference.PreferenceManager;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.ScrollView;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.SeekBar;
import android.widget.TextView;
import java.io.File;
import java.lang.Class;
import java.lang.Float;
import java.lang.Integer;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Timer;

public class activity extends android.app.Activity
{
   // constants
   private static final int maxProgress = 1000;

   private static final int MENU_PREFERENCES           = 1;
   private static final int MENU_QUIT                  = 2;
   private static final int MENU_RESET_PLAYLIST        = 3;
   private static final int MENU_SHARE                 = 4;
   private static final int MENU_COPY                  = 5;
   private static final int MENU_LINER                 = 6;
   private static final int MENU_UPDATE_PLAYLIST       = 7;
   private static final int MENU_DOWNLOAD_NEW_PLAYLIST = 8;
   private static final int MENU_JUMP_TO_SONG          = 9;
   private static final int MENU_SHOW_DOWNLOAD_LOG     = 10;
   private static final int MENU_SHOW_ERROR_LOG        = 11;

   private static final int RESULT_PREFERENCES = 1;

   // Main UI members

   private ImageView   albumArt;
   private TextView    artistTitle;
   private TextView    albumTitle;
   private TextView    songTitle;
   private TextView    currentTime;
   private TextView    totalTime;
   private ImageButton playPauseButton;
   private SeekBar     progressBar;
   private TextView    playlistTitle;
   private WakeLock    wakeLock;

   // Cached values
   private long          trackDuration = 0; // track duration in milliseconds
   private float         defaultTextViewTextSize; // set in onCreate
   private ComponentName playServiceComponentName; // set in OnCreate

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
         return Float.valueOf(scale);
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
      sendBroadcast
         (new Intent
          (utils.ACTION_COMMAND)
          .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_NOTE)
          .putExtra("note", ((String)((Button)v).getText()).replace('\n', ' ')));
   };

   private ImageButton.OnClickListener nextListener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_NEXT));
         }
      };

   private TextView.OnClickListener playlistListener = new TextView.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            PickPlaylistDialogFragment diag = new PickPlaylistDialogFragment();
            Bundle args = new Bundle();
            args.putInt("command", utils.COMMAND_PLAYLIST);
            diag.setArguments(args);
            diag.show(getFragmentManager(), "pick play playlist");
         }
      };

   private ImageButton.OnClickListener playPauseListener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_TOGGLEPAUSE));
         }
      };

   private ImageButton.OnClickListener prevListener = new ImageButton.OnClickListener()
      {
         @Override public void onClick(View v)
         {
            sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PREVIOUS));
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
                  (new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_SEEK).
                   putExtra(utils.EXTRA_COMMAND_POSITION, (trackDuration * progress / maxProgress)));
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
                  if (BuildConfig.DEBUG) utils.verboseLog("activity.onReceive META");

                  albumArt.setImageBitmap(utils.retriever.getAlbumArt()); // Ok if null

                  // On first start, with no playlist selected, these
                  // are all empty strings except playlist, which
                  // contains R.string.null_playlist or null_playlist_directory.
                  playlistTitle.setText(intent.getStringExtra("playlist"));
                  artistTitle.setText(utils.retriever.artist);
                  albumTitle.setText(utils.retriever.album);
                  songTitle.setText(utils.retriever.title);

                  trackDuration = Long.valueOf(utils.retriever.duration);

                  totalTime.setText(utils.makeTimeString(activity.this, trackDuration));
               }
               else if (action.equals(utils.PLAYSTATE_CHANGED))
               {
                  final boolean playing = intent.getBooleanExtra("playing", false);
                  final int currentPos = intent.getIntExtra("position", 0);

                  if (BuildConfig.DEBUG) utils.verboseLog("activity.onReceive PLAYSTATE");

                  if (playing)
                  {
                     playPauseButton.setImageResource(R.drawable.pause);
                  }
                  else
                  {
                     playPauseButton.setImageResource(R.drawable.play);
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
               utils.errorLog(null, "activity.broadcastReceiver: " + e);
            }
         }
      };

   private void setPlaylistDirectory()
   {
      Resources         res   = getResources();
      SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
      utils.playlistDirectory = prefs.getString
         (res.getString(R.string.playlist_directory_key),
          res.getString(R.string.playlist_directory_default));
   }

   private void setSMMDirectory()
   {
      Resources         res   = getResources();
      SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
      utils.smmDirectory = prefs.getString
         (res.getString(R.string.smm_directory_key),
          res.getString(R.string.smm_directory_default));
   }

   ////////// Activity lifetime methods (in lifecycle order)

   @Override public void onCreate(Bundle savedInstanceState)
   {
      try
      {
         super.onCreate(savedInstanceState);

         utils.mainActivity = this;

         PreferenceManager.setDefaultValues(this, R.xml.preferences, false);

         final float scale = getTextViewTextScale();

         final Intent intent = getIntent();

         setPlaylistDirectory();
         setSMMDirectory();

         utils.showDownloadLogIntent =
            new Intent(Intent.ACTION_VIEW)
            .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            .setDataAndType
            (new Uri.Builder()
             .scheme("file")
             .authority("")
             .path(DownloadUtils.logFileName())
             .build(),
             "text/plain");

         utils.showErrorLogIntent =
            new Intent(Intent.ACTION_VIEW)
            .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            .setDataAndType
            (new Uri.Builder()
             .scheme("file")
             .authority("")
             .path(utils.errorLogFileName())
             .build(),
             "text/plain");
         {
            GregorianCalendar time = new GregorianCalendar(); // holds current time
            time.add(Calendar.DAY_OF_MONTH, 1); // tomorrow
            time.set(Calendar.HOUR_OF_DAY, 0);
            time.set(Calendar.MINUTE, 30); // 12:30 AM

            if (null == utils.downloadTimer)
            {
               utils.downloadTimer = new Timer();
               utils.downloadTimer.scheduleAtFixedRate(utils.downloadTimerTask, time.getTime(), utils.millisPerDay);
            }
         }

         setContentView(R.layout.main);

         playServiceComponentName = startService (new Intent(this, PlayService.class));

         // Set up displays, top to bottom left to right

         albumArt    = (ImageView)findViewById(R.id.albumArt);
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
            sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_UPDATE_DISPLAY));
         }
         else
         {
            utils.errorLog(this, "onCreate got unexpected intent: " + intent.toString());
         }
      }
      catch (Exception e)
      {
         utils.errorLog(this, "activity.onCreate: exception: ", e);
         finish();
      }
   }

   @Override protected void onResume()
   {
      super.onResume();

      if (BuildConfig.DEBUG) utils.verboseLog("activity.onResume");

      try
      {
         Resources         res   = getResources();
         SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
         IntentFilter      f     = new IntentFilter();
         f.addAction(utils.META_CHANGED);
         f.addAction(utils.PLAYSTATE_CHANGED);
         registerReceiver(broadcastReceiver, f);
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_UPDATE_DISPLAY));

         if (prefs.getBoolean (res.getString(R.string.always_on_key), false))
         {
            wakeLock.acquire();
         }
      }
      catch (Exception e)
      {
         utils.errorLog(this, "activity.onResume: ", e);
      }
   }

   @Override protected void onPause()
   {
      super.onPause();

      if (BuildConfig.DEBUG) utils.verboseLog("activity.onPause");

      try
      {
         Resources         res   = getResources();
         SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
         if (prefs.getBoolean (res.getString(R.string.always_on_key), false))
         {
            wakeLock.release();
         }
         unregisterReceiver(broadcastReceiver);
      }
      catch (RuntimeException e)
      {
         utils.errorLog(this, "activity.onPause: ", e);
      }
   }

   ////////// key handling

   @Override public boolean onKeyDown(int keyCode, KeyEvent event)
   {
      boolean handled = false; // don't terminate event processing; let MediaEventReceivers get it

      switch (keyCode)
      {
         // Alphabetical keycode order
      case KeyEvent.KEYCODE_MEDIA_NEXT:
      case KeyEvent.KEYCODE_MEDIA_FAST_FORWARD:
         // Google TV Remote app has fast forward button but not next
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_NEXT));
         handled = true; // terminate event processing; MediaEventReceivers won't get it
         break;

      case KeyEvent.KEYCODE_MEDIA_PAUSE:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PAUSE));
         handled = true;
         break;

      case KeyEvent.KEYCODE_MEDIA_PLAY:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PLAY));
         handled = true;
         break;

      case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_TOGGLEPAUSE));
         handled = true;
         break;

      case KeyEvent.KEYCODE_MEDIA_PREVIOUS:
      case KeyEvent.KEYCODE_MEDIA_REWIND:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PREVIOUS));
         handled = true;
         break;

      case KeyEvent.KEYCODE_MEDIA_STOP:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_PAUSE));
         handled = true;
         break;

      default:
      }
      return handled;
   };

   ////////// Menu

   @Override public boolean onCreateOptionsMenu(Menu menu)
   {
      super.onCreateOptionsMenu(menu);
      menu.add(0, MENU_QUIT, 0, R.string.menu_quit);
      menu.add(0, MENU_SHARE, 0, R.string.menu_share);
      menu.add(0, MENU_LINER, 0, R.string.menu_liner);
      menu.add(0, MENU_COPY, 0, R.string.menu_copy);
      menu.add(0, MENU_RESET_PLAYLIST, 0, R.string.menu_reset_playlist);
      menu.add(0, MENU_UPDATE_PLAYLIST, 0, R.string.menu_update_playlist);
      menu.add(0, MENU_SHOW_DOWNLOAD_LOG, 0, R.string.menu_show_download_log);
      menu.add(0, MENU_SHOW_ERROR_LOG, 0, R.string.menu_show_error_log);
      menu.add(0, MENU_JUMP_TO_SONG, 0, R.string.menu_jump_to_song);
      menu.add(0, MENU_PREFERENCES, 0, R.string.menu_preferences);
      menu.add(0, MENU_DOWNLOAD_NEW_PLAYLIST, 0, R.string.menu_download_new_playlist);
      return true; // display menu
   }

   // @Override public boolean onPrepareOptionsMenu(Menu menu)
   // {
   //    // The Android documentation says this is called just
   //    // before the menu is displayed. It lies; this is called once
   //    // after onCreateOptionsMenu!
   //    super.onPrepareOptionsMenu(menu);

   //    menu.findItem(MENU_LINER).setVisible(utils.retriever.linerNotesExist());

   //    return true;
   // }

   @Override public boolean onOptionsItemSelected(MenuItem item)
   {
      switch (item.getItemId())
      {
      case MENU_QUIT:
         sendBroadcast
            (new Intent
             (utils.ACTION_COMMAND)
             .putExtra(utils.EXTRA_COMMAND, utils.COMMAND_QUIT));

         stopService (new Intent().setComponent(playServiceComponentName));

         finish();
         break;

      case MENU_DOWNLOAD_NEW_PLAYLIST:
         {
            Resources         res      = getResources();
            SharedPreferences prefs    = PreferenceManager.getDefaultSharedPreferences(this);
            String            serverIP = prefs.getString (res.getString(R.string.server_IP_key), null);

            if (null == serverIP)
               utils.alertLog(this, "set Server IP in preferences");
            else
            {
               TextDialogFragment diag = new TextDialogFragment();
               Bundle args = new Bundle();
               args.putInt("command", utils.COMMAND_DOWNLOAD);
               diag.setArguments(args);
               diag.show(getFragmentManager(), "enter new playlist category");
            }
         }
         break;

      case MENU_UPDATE_PLAYLIST:
         {
            Resources         res      = getResources();
            SharedPreferences prefs    = PreferenceManager.getDefaultSharedPreferences(this);
            String            serverIP = prefs.getString (res.getString(R.string.server_IP_key), null);

            if (null == serverIP)
               utils.alertLog(this, "set Server IP in preferences");
            else
            {
               PickPlaylistDialogFragment diag = new PickPlaylistDialogFragment();
               Bundle args = new Bundle();
               args.putInt("command", utils.COMMAND_DOWNLOAD);
               diag.setArguments(args);
               diag.show(getFragmentManager(), "pick update playlist");
            }
         }
         break;

      case MENU_JUMP_TO_SONG:
         {
               TextDialogFragment diag = new TextDialogFragment();
               Bundle args = new Bundle();
               args.putInt("command", utils.COMMAND_JUMP);
               diag.setArguments(args);
               diag.show(getFragmentManager(), "enter song to jump to");
         }
         break;

      case MENU_SHOW_DOWNLOAD_LOG:
         startActivity(utils.showDownloadLogIntent);
         break;

      case MENU_SHOW_ERROR_LOG:
         startActivity(utils.showErrorLogIntent);
         break;

      case MENU_PREFERENCES:
         startActivityForResult (new Intent(this, preferences.class), RESULT_PREFERENCES);
         break;

      case MENU_RESET_PLAYLIST:
         sendBroadcast(new Intent(utils.ACTION_COMMAND).putExtra(utils.EXTRA_COMMAND, utils.COMMAND_RESET_PLAYLIST));
         break;

      case MENU_SHARE:
         {
            utils.verboseLog("sharing " + utils.retriever.musicUri.toString());

            Intent intent = new Intent()
               .setAction(Intent.ACTION_SEND)
               .putExtra(Intent.EXTRA_STREAM, utils.retriever.musicUri)
               .setType("audio/mp3");

            startActivity(Intent.createChooser(intent, "Share song via ..."));
         }
         break;

      case MENU_COPY:
         {
            ClipboardManager clipManage = (ClipboardManager) getSystemService(CLIPBOARD_SERVICE);

            clipManage.setPrimaryClip
               (ClipData.newPlainText
                ("song", artistTitle.getText() + " " + albumTitle.getText() + " " + songTitle.getText()));
         }
         break;

      case MENU_LINER:
         {
            Intent intent = new Intent(Intent.ACTION_VIEW)
               .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
               .setDataAndType(utils.retriever.linerUri, "application/pdf");

            startActivity(Intent.createChooser(intent, "Show liner notes via ..."));
         }
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

         case utils.RESULT_SMM_DIRECTORY:
            {
               setSMMDirectory();
               // value from preferences
            }
            break;

         case utils.RESULT_PLAYLIST_DIRECTORY:
            {
               setPlaylistDirectory();
               // value from preferences
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
